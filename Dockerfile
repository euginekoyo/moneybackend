# Use SWI-Prolog's latest image
FROM swipl:latest

# Set the working directory
WORKDIR /mentor

# Copy the Prolog files into the container
COPY . .

# Expose the port your server is running on
EXPOSE 8000

# Start the Prolog server and ensure it loads app.pl
CMD ["swipl", "-s", "mentor.pl", "-g", "start_server(8000)", "-t", "halt"]
