// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";

contract FundManager is ReentrancyGuard {
    struct Issue {
        uint bounty;
        bool allocated;
        address funder;
        bool closed;
    }

    mapping(uint => Issue) public issues;

    event FundsDeposited(address indexed funder, uint amount);
    event BountyAllocated(uint issueId, uint bounty, address indexed funder);
    event IssueClosed(uint issueId);

    // Funders deposit funds into the contract.
    function depositFunds() public payable {
        require(msg.value > 0, "Must deposit non-zero funds");
        emit FundsDeposited(msg.sender, msg.value);
    }

    // Funders allocate a bounty to a GitLab issue.
    function allocateBounty(uint issueId, uint bounty) public {
        require(bounty > 0, "Bounty must be > 0");
        issues[issueId] = Issue(bounty, true, msg.sender, false);
        emit BountyAllocated(issueId, bounty, msg.sender);
    }

    // Mark an issue as closed (assume GitLab merge confirmed).
    function closeIssue(uint issueId) public {
        require(issues[issueId].allocated, "Bounty not allocated");
        issues[issueId].closed = true;
        emit IssueClosed(issueId);
    }

    // Allow the contract to receive funds.
    receive() external payable {}
}
