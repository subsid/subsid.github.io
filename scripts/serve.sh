#!/bin/bash

# Blog server script - serves either public or private blog
# Usage: ./serve.sh [private] [port]
# Default port is 8000

PORT=${2:-8000}

if [ "$1" = "private" ]; then
    BLOG_DIR="./private"
    BLOG_TYPE="private"
else
    BLOG_DIR="./public"
    BLOG_TYPE="public"
fi

# Check if directory exists
if [ ! -d "$BLOG_DIR" ]; then
    echo "Error: $BLOG_TYPE blog directory '$BLOG_DIR' does not exist."
    echo "Please build the blog first."
    exit 1
fi

echo "Starting $BLOG_TYPE blog server..."
echo "Directory: $BLOG_DIR"
echo "URL: http://localhost:$PORT"
echo "Press Ctrl+C to stop the server"
echo

cd "$BLOG_DIR" && python -m http.server "$PORT"
