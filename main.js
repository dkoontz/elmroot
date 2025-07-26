import http from "http";
import { v4 as uuidv4 } from "uuid";
import pino from "pino";
import * as TaskPort from "elm-taskport/js/taskport.js";
import { XMLHttpRequest } from "xmlhttprequest";
import { Elm } from "./dist/main.mjs";
import pg from "pg";

const elmFlags = { postgrestHost: process.env.PGREST_HOST || "localhost" };
const taskPortSettings = {};
const pgConnection = new pg.Client();

// Configure Pino logger
const logger = pino({
  level: process.env.LOG_LEVEL || "info",
  transport: {
    target: "pino-pretty",
    options: {
      colorize: true,
      translateTime: "HH:MM:ss.l",
      ignore: "pid,hostname",
      messageFormat: "{levelLabel} - {msg}",
    },
  },
});

setupErrorHandlers(process);

global.XMLHttpRequest = function () {
  XMLHttpRequest.call(this);
  TaskPort.install(taskPortSettings, this);
};

setupTaskPortHandlers(TaskPort, pgConnection);

// Initialize the Elm application
const app = Elm.Main.init(elmFlags);

// Store pending HTTP responses
const pendingResponses = new Map();

// Server reference for graceful shutdown
let server;

////// Listen for Elm commands via ports //////

app.ports.sendResponse.subscribe((response) => {
  try {
    const { id, status, body, headers } = response;
    const res = pendingResponses.get(id);

    logger.info({ requestId: id, status }, "Response received from Elm");

    if (res) {
      if (res.headersSent) {
        logger.warn(
          { requestId: id },
          "Headers already sent, cannot send response"
        );
        return;
      }

      // Set headers safely
      headers.forEach(([name, value]) => {
        try {
          res.setHeader(name, value);
        } catch (error) {
          logger.error(
            { error: error.message, name, value },
            "Failed to set header"
          );
        }
      });

      res.statusCode = status;
      res.end(body);
      pendingResponses.delete(id);
      logger.debug({ requestId: id }, "Response sent successfully");
    } else {
      logger.error(
        { requestId: id },
        "No pending response found for request ID"
      );
    }
  } catch (error) {
    logger.error(
      { error: error.message, stack: error.stack },
      "Error in sendResponse port handler"
    );
  }
});

app.ports.logging.subscribe((message) => {
  try {
    logger.info({ source: "elm" }, message);
  } catch (error) {
    logger.error({ error: error.message }, "Error in logging port handler");
  }
});

////////////

// Create HTTP server with error handling
server = http.createServer((req, res) => {
  const requestId = uuidv4();

  try {
    logger.info(
      {
        requestId,
        method: req.method,
        url: req.url,
        userAgent: req.headers["user-agent"],
      },
      "HTTP request received"
    );

    // Store response object for later use
    pendingResponses.set(requestId, res);

    // Set up error handling for the response
    res.on("error", (error) => {
      logger.error(
        {
          requestId,
          error: error.message,
          stack: error.stack,
        },
        "HTTP response error"
      );
      pendingResponses.delete(requestId);
    });

    // Read request body
    let buffer = [];
    let bufferLength = 0;
    const maxBodySize = 10 * 1024 * 1024; // 10MB limit

    req.on("data", (chunk) => {
      try {
        bufferLength += chunk.length;
        if (bufferLength > maxBodySize) {
          logger.warn({ requestId }, "Request body too large");
          res.statusCode = 413;
          res.end("Request Entity Too Large");
          pendingResponses.delete(requestId);
          return;
        }
        buffer.push(chunk);
      } catch (error) {
        logger.error(
          { requestId, error: error.message },
          "Error reading request data"
        );
        res.statusCode = 400;
        res.end("Bad Request");
        pendingResponses.delete(requestId);
      }
    });

    req.on("end", () => {
      try {
        if (res.headersSent) {
          logger.warn({ requestId }, "Response already sent");
          return;
        }

        // Extract headers
        const headers = Object.entries(req.headers).map(([name, value]) => ({
          name: name,
          value: value,
        }));

        const body = Buffer.concat(buffer).toString("utf8");

        // Send request to Elm
        const httpRequest = {
          id: requestId,
          method: req.method,
          url: constructFullUrl(req),
          body: body,
          headers: headers,
        };

        logger.debug(
          { requestId, bodyLength: body.length },
          "Sending request to Elm"
        );
        app.ports.httpRequest.send(httpRequest);
      } catch (error) {
        logger.error(
          {
            requestId,
            error: error.message,
            stack: error.stack,
          },
          "Error processing request end"
        );

        if (!res.headersSent) {
          res.statusCode = 500;
          res.end(
            JSON.stringify({
              error: `Internal Server Error processing request: ${requestId}`,
              message: error.message,
            })
          );
        }
        pendingResponses.delete(requestId);
      }
    });

    req.on("error", (error) => {
      logger.error(
        {
          requestId,
          error: error.message,
          stack: error.stack,
        },
        "HTTP request error"
      );

      if (!res.headersSent) {
        res.statusCode = 400;
        res.end("Bad Request");
      }
      pendingResponses.delete(requestId);
    });

    // Handle request timeout
    const timeout = setTimeout(() => {
      if (pendingResponses.has(requestId)) {
        logger.warn({ requestId }, "Request timeout");
        res.statusCode = 504;
        res.end("Gateway Timeout");
        pendingResponses.delete(requestId);
      }
    }, 60000); // 60 second timeout

    // Clear timeout if response is sent
    res.on("finish", () => {
      clearTimeout(timeout);
    });
  } catch (error) {
    logger.error(
      {
        requestId,
        error: error.message,
        stack: error.stack,
      },
      "Unexpected error in request handler"
    );

    if (!res.headersSent) {
      res.statusCode = 500;
      res.end("Internal Server Error");
    }
    pendingResponses.delete(requestId);
  }
});

// Add server-level error handling
server.on("error", (error) => {
  logger.fatal({ error: error.message, stack: error.stack }, "Server error");
  process.exit(1);
});

server.on("clientError", (error, socket) => {
  logger.error({ error: error.message }, "Client error");
  if (socket.writable) {
    socket.end("HTTP/1.1 400 Bad Request\r\n\r\n");
  }
});

const PORT = process.env.ELMROOT_PORT || 3000;
server.listen(PORT, () => {
  logger.info({ port: PORT }, "Server started successfully");
});

// Add startup logging
logger.info(
  {
    nodeVersion: process.version,
    platform: process.platform,
    processId: process.pid,
    env: process.env.NODE_ENV || "development",
  },
  "Application starting"
);

// Helper function to construct full URL from request
function constructFullUrl(req) {
  // Get protocol (http vs https)
  const protocol = req.connection.encrypted ? "https" : "http";

  // Get host header (may or may not include port)
  const hostHeader = req.headers.host;

  // Check if host already includes port
  if (hostHeader && hostHeader.includes(":")) {
    // Host header already has port (e.g., "localhost:3000")
    return `${protocol}://${hostHeader}${req.url}`;
  } else {
    // No port in host header, need to determine if we should add one
    const defaultPort = protocol === "https" ? 443 : 80;
    const actualPort = req.socket.localPort;

    if (actualPort && actualPort !== defaultPort) {
      // Running on non-standard port, include it
      return `${protocol}://${hostHeader}:${actualPort}${req.url}`;
    } else {
      // Standard port, don't include it
      return `${protocol}://${hostHeader}${req.url}`;
    }
  }
}

// Graceful shutdown handlers
const gracefulShutdown = (signal) => {
  logger.info(`Received ${signal}. Starting graceful shutdown...`);

  if (server) {
    server.close((err) => {
      if (err) {
        logger.error({ error: err.message }, "Error during server shutdown");
        process.exit(1);
      }
      logger.info("Server closed successfully");
      process.exit(0);
    });

    // Force shutdown after 30 seconds
    setTimeout(() => {
      logger.error("Forced shutdown after timeout");
      process.exit(1);
    }, 30000);
  } else {
    process.exit(0);
  }
};

function setupErrorHandlers(process) {
  // Global error handlers
  process.on("uncaughtException", (error) => {
    logger.fatal(
      { error: error.message, stack: error.stack },
      "Uncaught Exception"
    );
    process.exit(1);
  });

  process.on("unhandledRejection", (reason, promise) => {
    logger.fatal({ reason, promise }, "Unhandled Promise Rejection");
    process.exit(1);
  });

  process.on("warning", (warning) => {
    logger.warn(
      {
        name: warning.name,
        message: warning.message,
        stack: warning.stack,
      },
      "Node.js Warning"
    );
  });

  process.on("SIGTERM", () => gracefulShutdown("SIGTERM"));
  process.on("SIGINT", () => gracefulShutdown("SIGINT"));
}

function setupTaskPortHandlers(taskPort, pgConnection) {
  taskPort.register("executeSqlQuery", (query) => {
    return Promise.resolve({
      interpolatedQuery: pgConnection.interpolatedQuery(
        query.query,
        query.values
      ),
    });
  });
}
