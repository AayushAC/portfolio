// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

import "./FundManager.sol";
import "./ValidatorMultiSig.sol";
import "@openzeppelin/contracts/utils/ReentrancyGuard.sol";

contract DeveloperPayouts is ReentrancyGuard {
    FundManager public fundManager;
    ValidatorMultiSig public multiSig;

    uint public payoutCount;

    struct Payout {
        uint issueId;
        address developer;
        bool paid;
    }

    mapping(uint => Payout) public payouts;

    event PayoutRequested(uint payoutId, uint issueId, address developer);
    event PayoutReleased(uint payoutId, uint issueId, address developer);

    constructor(address _fundManager, address _multiSig) {
        fundManager = FundManager(payable(_fundManager)); // ✅ Explicitly mark as payable
        multiSig = ValidatorMultiSig(_multiSig);
    }

    function requestPayout(uint issueId) public nonReentrant {
        (uint bounty, bool allocated, , bool closed) = fundManager.issues(issueId);
        require(allocated, "Issue not allocated");
        require(closed, "Issue not closed yet");

        payouts[payoutCount] = Payout(issueId, msg.sender, false);
        emit PayoutRequested(payoutCount, issueId, msg.sender);
        payoutCount++;
    }

    function releasePayout(uint payoutId) public nonReentrant {
        require(!payouts[payoutId].paid, "Payout already released");
        require(multiSig.isApproved(payoutId), "Insufficient validator approvals");

        (uint bounty, , , ) = fundManager.issues(payouts[payoutId].issueId);
        require(address(this).balance >= bounty, "Insufficient contract balance");

        payouts[payoutId].paid = true;
        payable(payouts[payoutId].developer).transfer(bounty);
        emit PayoutReleased(payoutId, payouts[payoutId].issueId, payouts[payoutId].developer);
    }

    receive() external payable {}
}
