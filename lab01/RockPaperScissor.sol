//RUN THIS IN CONSOLE TO GENERATE A HASH FOR VOTE AND SALT : web3.utils.keccak256(web3.utils.encodePacked("paper", "secret2"))

// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract RockPaperScissors {
    event Played(address indexed player, bytes32 hashedVote);
    event Revealed(address indexed player, string vote);
    event GameResult(address indexed player1, address indexed player2, string vote1, string vote2, address winner);
    event Withdrawn(address indexed player, uint256 amount);

    uint256 public constant MIN_STAKE = 0.01 ether;
    uint256 public constant REVEAL_TIMEOUT = 5 minutes;

    address public player1;
    address public player2;
    bytes32 public hashedVote1;
    bytes32 public hashedVote2;
    string public vote1;
    string public vote2;
    uint256 public stake;
    uint256 public revealDeadline;
    address public winner;
    bool public gameComplete;

    mapping(address => uint256) public pendingWithdrawals;

    function play(bytes32 _hashedVote) public payable {
        require(msg.value >= MIN_STAKE, "Stake too low");
        require(player1 == address(0) || player2 == address(0), "Game full");
        require(msg.sender != player1, "Cannot play twice");

        if (player1 == address(0)) {
            player1 = msg.sender;
            hashedVote1 = _hashedVote;
            stake = msg.value;
            emit Played(msg.sender, _hashedVote);
        } else {
            require(msg.value == stake, "Stake mismatch");
            player2 = msg.sender;
            hashedVote2 = _hashedVote;
            revealDeadline = block.timestamp + REVEAL_TIMEOUT;
            emit Played(msg.sender, _hashedVote);
        }
    }

    function reveal(string memory _vote, string memory _salt) public {
        require(player1 != address(0) && player2 != address(0), "Game not started");
        require(block.timestamp <= revealDeadline, "Reveal period expired");

        bytes32 computedHash = keccak256(abi.encodePacked(_vote, _salt));

        if (msg.sender == player1) {
            require(hashedVote1 == computedHash, "Invalid reveal");
            vote1 = _vote;
            emit Revealed(msg.sender, _vote);
        } else if (msg.sender == player2) {
            require(hashedVote2 == computedHash, "Invalid reveal");
            vote2 = _vote;
            emit Revealed(msg.sender, _vote);
        } else {
            revert("Invalid player");
        }

        if (bytes(vote1).length > 0 && bytes(vote2).length > 0) {
            _determineWinner();
        }
    }

    function _determineWinner() internal {
        bytes32 move1 = keccak256(abi.encodePacked(vote1));
        bytes32 move2 = keccak256(abi.encodePacked(vote2));

        if (move1 == move2) {
            winner = address(0); // Draw, no winner
        } else if (
            (move1 == keccak256("rock") && move2 == keccak256("scissors")) ||
            (move1 == keccak256("scissors") && move2 == keccak256("paper")) ||
            (move1 == keccak256("paper") && move2 == keccak256("rock"))
        ) {
            winner = player1;
            pendingWithdrawals[winner] += stake * 2;
        } else {
            winner = player2;
            pendingWithdrawals[winner] += stake * 2;
        }

        gameComplete = true;
        emit GameResult(player1, player2, vote1, vote2, winner);
    }

    function withdraw() public {
        require(gameComplete, "Game not complete");
        require(pendingWithdrawals[msg.sender] > 0, "Nothing to withdraw");

        uint256 amount = pendingWithdrawals[msg.sender];
        pendingWithdrawals[msg.sender] = 0;

        (bool success, ) = payable(msg.sender).call{value: amount}("");
        require(success, "Withdraw failed");

        emit Withdrawn(msg.sender, amount);
    }

    function claimTimeout() public {
        require(block.timestamp > revealDeadline, "Reveal period not over");
        require(!gameComplete, "Game already completed");

        if (bytes(vote1).length == 0) {
            pendingWithdrawals[player2] += stake * 2;
            winner = player2;
        } else if (bytes(vote2).length == 0) {
            pendingWithdrawals[player1] += stake * 2;
            winner = player1;
        } else {
            revert("Both players revealed");
        }

        gameComplete = true;
        emit GameResult(player1, player2, vote1, vote2, winner);
    }
}

