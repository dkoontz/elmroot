import http from "http";
import { v4 as uuidv4 } from "uuid";
import { Elm } from "./dist/main.mjs";

const flags = {};

// Initialize the Elm application
const app = Elm.Main.init(flags);

// Store pending HTTP responses
const pendingResponses = new Map();

////// Listen for Elm commands via ports //////

app.ports.sendResponse.subscribe((response) => {
  const { id, status, body, headers } = response;
  const res = pendingResponses.get(id);

  if (res) {
    // Set headers
    headers.forEach(([key, value]) => {
      res.setHeader(key, value);
    });

    res.statusCode = status;
    res.end(body);
    pendingResponses.delete(id);
  }
});

app.ports.logging.subscribe((message) => {
  console.log(message);
});

////////////

// Create HTTP server
const server = http.createServer((req, res) => {
  const requestId = uuidv4();

  // Store response object for later use
  pendingResponses.set(requestId, res);

  // Read request body
  let buffer = [];
  req.on("data", (chunk) => {
    buffer.push(chunk);
  });

  req.on("end", () => {
    if (res.complete) {
      // Extract headers
      const headers = Object.entries(req.headers).map(([key, value]) => ({
        key: key,
        value: value,
      }));

      const body = Buffer.concat(buffer).toString("utf8");

      // Send request to Elm
      const httpRequest = {
        id: requestId,
        method: req.method,
        url: req.url,
        body: body,
        headers: headers,
      };

      app.ports.httpRequest.send(httpRequest);
    } else {
      // log 'The connection was terminated while the message was still being sent'
      res.end();
    }
  });

  // Handle request timeout
  setTimeout(() => {
    if (pendingResponses.has(requestId)) {
      res.statusCode = 504;
      res.end("Internal Server Error: Request timed out");
      pendingResponses.delete(requestId);
    }
  }, 60000); // 60 second timeout
});

const PORT = process.env.ELMROOT_PORT || 3000;
server.listen(PORT, () => {
  console.log(`Server running on port ${PORT}`);
});
