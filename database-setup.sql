# Create the database tables needed to use Schwordpress, if they
# don't already exist.

CREATE DATABASE IF NOT EXISTS schwordpress;
USE schwordpress;
CREATE TABLE IF NOT EXISTS `posts` (
  `title` varchar(255) DEFAULT NULL,
  `timestamp` datetime DEFAULT NULL,
  `content` text);
