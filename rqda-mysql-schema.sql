-- RQDA MySQL Database Schema
-- Adapted from SQLite original schema

-- Create database (run this separately if needed)
-- CREATE DATABASE rqda_online CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
-- USE rqda_online;

-- Project table - contains information about the project and *.rqda file
CREATE TABLE IF NOT EXISTS project (
    id INT AUTO_INCREMENT PRIMARY KEY,
    databaseversion VARCHAR(20) DEFAULT '0.2.2',
    date DATETIME DEFAULT CURRENT_TIMESTAMP,
    dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    memo TEXT,
    about TEXT,
    owner VARCHAR(255),
    status TINYINT DEFAULT 1,
    project_name VARCHAR(255) NOT NULL,
    created_by VARCHAR(255),
    INDEX idx_owner (owner),
    INDEX idx_status (status)
);

-- Source table - contains the content of files
CREATE TABLE IF NOT EXISTS source (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    file LONGTEXT NOT NULL,
    memo TEXT,
    owner VARCHAR(255),
    date DATETIME DEFAULT CURRENT_TIMESTAMP,
    dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    status TINYINT DEFAULT 1,
    project_id INT,
    FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
    INDEX idx_project_status (project_id, status),
    INDEX idx_owner (owner),
    FULLTEXT(file, name)
);

-- File categories table
CREATE TABLE IF NOT EXISTS filecat (
    catid INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    memo TEXT,
    owner VARCHAR(255),
    date DATETIME DEFAULT CURRENT_TIMESTAMP,
    dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    status TINYINT DEFAULT 1,
    project_id INT,
    FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
    INDEX idx_project_status (project_id, status)
);

-- Free codes table - contains information on the codes list
CREATE TABLE IF NOT EXISTS freecode (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    memo TEXT,
    owner VARCHAR(255),
    date DATETIME DEFAULT CURRENT_TIMESTAMP,
    dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    status TINYINT DEFAULT 1,
    color VARCHAR(7), -- hex color code
    project_id INT,
    FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
    INDEX idx_project_status (project_id, status),
    INDEX idx_name (name)
);

-- Code categories table
CREATE TABLE IF NOT EXISTS codecat (
    catid INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    memo TEXT,
    owner VARCHAR(255),
    date DATETIME DEFAULT CURRENT_TIMESTAMP,
    dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    status TINYINT DEFAULT 1,
    project_id INT,
    FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
    INDEX idx_project_status (project_id, status)
);

-- Tree code table - codes categorization (relationship between codes and codecat)
CREATE TABLE IF NOT EXISTS treecode (
    cid INT,
    catid INT,
    memo TEXT,
    date DATETIME DEFAULT CURRENT_TIMESTAMP,
    dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    owner VARCHAR(255),
    status TINYINT DEFAULT 1,
    project_id INT,
    PRIMARY KEY (cid, catid),
    FOREIGN KEY (cid) REFERENCES freecode(id) ON DELETE CASCADE,
    FOREIGN KEY (catid) REFERENCES codecat(catid) ON DELETE CASCADE,
    FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
    INDEX idx_project_status (project_id, status)
);

-- Tree file table - file categorization (relation between source files and filecat)
CREATE TABLE IF NOT EXISTS treefile (
    fid INT,
    catid INT,
    memo TEXT,
    date DATETIME DEFAULT CURRENT_TIMESTAMP,
    dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    owner VARCHAR(255),
    status TINYINT DEFAULT 1,
    project_id INT,
    PRIMARY KEY (fid, catid),
    FOREIGN KEY (fid) REFERENCES source(id) ON DELETE CASCADE,
    FOREIGN KEY (catid) REFERENCES filecat(catid) ON DELETE CASCADE,
    FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
    INDEX idx_project_status (project_id, status)
);

-- Coding table - contains information on codings
CREATE TABLE IF NOT EXISTS coding (
    rowid INT AUTO_INCREMENT PRIMARY KEY,
    cid INT NOT NULL,
    fid INT NOT NULL,
    seltext TEXT,
    selfirst INT,
    selend INT,
    status TINYINT DEFAULT 1,
    owner VARCHAR(255),
    date DATETIME DEFAULT CURRENT_TIMESTAMP,
    dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    memo TEXT,
    project_id INT,
    FOREIGN KEY (cid) REFERENCES freecode(id) ON DELETE CASCADE,
    FOREIGN KEY (fid) REFERENCES source(id) ON DELETE CASCADE,
    FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
    INDEX idx_cid_fid (cid, fid),
    INDEX idx_project_status (project_id, status),
    INDEX idx_position (fid, selfirst, selend),
    FULLTEXT(seltext)
);

-- Annotation table - contains file annotations
CREATE TABLE IF NOT EXISTS annotation (
    rowid INT AUTO_INCREMENT PRIMARY KEY,
    fid INT NOT NULL,
    position INT,
    annotation TEXT,
    owner VARCHAR(255),
    date DATETIME DEFAULT CURRENT_TIMESTAMP,
    dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    status TINYINT DEFAULT 1,
    project_id INT,
    FOREIGN KEY (fid) REFERENCES source(id) ON DELETE CASCADE,
    FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
    INDEX idx_fid_position (fid, position),
    INDEX idx_project_status (project_id, status)
);

-- Cases table - contains information about case list
CREATE TABLE IF NOT EXISTS cases (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    memo TEXT,
    owner VARCHAR(255),
    date DATETIME DEFAULT CURRENT_TIMESTAMP,
    dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    status TINYINT DEFAULT 1,
    project_id INT,
    FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
    INDEX idx_project_status (project_id, status)
);

-- Case attributes table
CREATE TABLE IF NOT EXISTS caseAttr (
    variable VARCHAR(255),
    value TEXT,
    caseId INT,
    date DATETIME DEFAULT CURRENT_TIMESTAMP,
    dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    owner VARCHAR(255),
    status TINYINT DEFAULT 1,
    project_id INT,
    PRIMARY KEY (variable, caseId),
    FOREIGN KEY (caseId) REFERENCES cases(id) ON DELETE CASCADE,
    FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
    INDEX idx_project_status (project_id, status)
);

-- Case linkage table - relationship between case and files
CREATE TABLE IF NOT EXISTS caselinkage (
    caseid INT,
    fid INT,
    owner VARCHAR(255),
    date DATETIME DEFAULT CURRENT_TIMESTAMP,
    dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    status TINYINT DEFAULT 1,
    project_id INT,
    PRIMARY KEY (caseid, fid),
    FOREIGN KEY (caseid) REFERENCES cases(id) ON DELETE CASCADE,
    FOREIGN KEY (fid) REFERENCES source(id) ON DELETE CASCADE,
    FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
    INDEX idx_project_status (project_id, status)
);

-- Attributes table - name list of attributes
CREATE TABLE IF NOT EXISTS attributes (
    name VARCHAR(255) PRIMARY KEY,
    status TINYINT DEFAULT 1,
    memo TEXT,
    owner VARCHAR(255),
    date DATETIME DEFAULT CURRENT_TIMESTAMP,
    dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    class VARCHAR(50) DEFAULT 'character',
    project_id INT,
    FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
    INDEX idx_project_status (project_id, status)
);

-- File attributes table
CREATE TABLE IF NOT EXISTS fileAttr (
    variable VARCHAR(255),
    value TEXT,
    fid INT,
    date DATETIME DEFAULT CURRENT_TIMESTAMP,
    dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    owner VARCHAR(255),
    status TINYINT DEFAULT 1,
    project_id INT,
    PRIMARY KEY (variable, fid),
    FOREIGN KEY (fid) REFERENCES source(id) ON DELETE CASCADE,
    FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
    INDEX idx_project_status (project_id, status)
);

-- Image table (for future use)
CREATE TABLE IF NOT EXISTS image (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255),
    imagepath TEXT,
    memo TEXT,
    owner VARCHAR(255),
    date DATETIME DEFAULT CURRENT_TIMESTAMP,
    dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    status TINYINT DEFAULT 1,
    project_id INT,
    FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
    INDEX idx_project_status (project_id, status)
);

-- Image coding table
CREATE TABLE IF NOT EXISTS imageCoding (
    cid INT,
    iid INT,
    x1 INT,
    y1 INT,
    x2 INT,
    y2 INT,
    memo TEXT,
    date DATETIME DEFAULT CURRENT_TIMESTAMP,
    dateM DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    owner VARCHAR(255),
    status TINYINT DEFAULT 1,
    project_id INT,
    FOREIGN KEY (cid) REFERENCES freecode(id) ON DELETE CASCADE,
    FOREIGN KEY (iid) REFERENCES image(id) ON DELETE CASCADE,
    FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
    INDEX idx_project_status (project_id, status)
);

-- Users table for authentication (not in original RQDA)
CREATE TABLE IF NOT EXISTS users (
    id INT AUTO_INCREMENT PRIMARY KEY,
    username VARCHAR(255) UNIQUE NOT NULL,
    email VARCHAR(255) UNIQUE NOT NULL,
    password_hash VARCHAR(255) NOT NULL,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    last_login DATETIME,
    active TINYINT DEFAULT 1,
    INDEX idx_username (username),
    INDEX idx_email (email)
);

-- Project permissions table (not in original RQDA)
CREATE TABLE IF NOT EXISTS project_permissions (
    project_id INT,
    user_id INT,
    permission_level ENUM('owner', 'editor', 'viewer') DEFAULT 'viewer',
    granted_by INT,
    granted_at DATETIME DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (project_id, user_id),
    FOREIGN KEY (project_id) REFERENCES project(id) ON DELETE CASCADE,
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
    FOREIGN KEY (granted_by) REFERENCES users(id)
);
