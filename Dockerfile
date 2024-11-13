# Use SWI-Prolog's latest image
FROM swipl:latest

# Set the working directory
WORKDIR /mentor

# Copy the Prolog files into the container
COPY . .

# Expose the port your server is running on
EXPOSE 8081

# Start the Prolog server and ensure it loads mentor.pl
CMD ["swipl", "-s", "mentor.pl", "-g", "start_server", "-t", "halt"]
