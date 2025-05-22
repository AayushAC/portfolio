// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract ValidatorMultiSig {
    address[] public validators;
    uint public requiredConfirmations;

    mapping(uint => mapping(address => bool)) public confirmations;

    event PaymentApproved(uint payoutId, address validator);

    constructor(address[] memory _validators, uint _requiredConfirmations) {
        require(_validators.length >= _requiredConfirmations, "Insufficient validators");
        validators = _validators;
        requiredConfirmations = _requiredConfirmations;
    }

    function approvePayment(uint payoutId) public {
        require(isValidator(msg.sender), "Not an authorized validator");
        require(!confirmations[payoutId][msg.sender], "Already confirmed");

        confirmations[payoutId][msg.sender] = true;
        emit PaymentApproved(payoutId, msg.sender);
    }

    function isApproved(uint payoutId) public view returns (bool) {
        uint count = 0;
        for (uint i = 0; i < validators.length; i++) {
            if (confirmations[payoutId][validators[i]]) {
                count++;
            }
        }
        return count >= requiredConfirmations;
    }

    function isValidator(address _addr) internal view returns (bool) {
        for (uint i = 0; i < validators.length; i++) {
            if (validators[i] == _addr) {
                return true;
            }
        }
        return false;
    }
}
