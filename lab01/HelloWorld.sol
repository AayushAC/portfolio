// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract HelloName {
    event Hello(string message);

    function hello(string memory name) public {
        string memory greeting = string(abi.encodePacked("Hello, ", name));
        emit Hello(greeting);
    }
}
