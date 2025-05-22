# REST API Server in Haskell

A simple REST API server implemented in Haskell using Servant.

## Endpoints

- `GET /hello` - Returns "Hello, World!"
- `GET /greet/{name}` - Returns a JSON greeting with the provided name
- `POST /greetme` - Accepts JSON input and returns a formatted message
- `GET /announcements` - Returns course announcements (format=html for HTML, default is JSON)
- `GET /` - Returns 404 error

## Building and Running

```
stack build
stack exec lab09-haskell-exe
```

The server will be available at http://localhost:8080

# Test the hello endpoint
curl http://localhost:8080/hello

# Test the greet endpoint
curl http://localhost:8080/greet/YourName

# Test the greetme endpoint
curl -X POST -H "Content-Type: application/json" -d '{"input":"Hello", "name":"World"}' http://localhost:8080/greetme

# Get announcements as JSON
curl http://localhost:8080/announcements.json

# Get announcements as HTML
curl http://localhost:8080/announcements.html