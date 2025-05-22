let provider;
let signer;
let fundManager;
let validatorMultiSig;
let developerPayouts;

const fundManagerAddress = "0xA51c1fc2f0D1a1b8494Ed1FE312d7C3a78Ed91C0";
const validatorMultiSigAddress = "0x0DCd1Bf9A1b36cE34237eEaFef220932846BCD82";
const developerPayoutsAddress = "0x9A676e781A523b5d0C0e43731313A708CB607508";


const fundManagerABI = [
    {
        "inputs": [],
        "name": "ReentrancyGuardReentrantCall",
        "type": "error"
      },
      {
        "anonymous": false,
        "inputs": [
          {
            "indexed": false,
            "internalType": "uint256",
            "name": "issueId",
            "type": "uint256"
          },
          {
            "indexed": false,
            "internalType": "uint256",
            "name": "bounty",
            "type": "uint256"
          },
          {
            "indexed": true,
            "internalType": "address",
            "name": "funder",
            "type": "address"
          }
        ],
        "name": "BountyAllocated",
        "type": "event"
      },
      {
        "anonymous": false,
        "inputs": [
          {
            "indexed": true,
            "internalType": "address",
            "name": "funder",
            "type": "address"
          },
          {
            "indexed": false,
            "internalType": "uint256",
            "name": "amount",
            "type": "uint256"
          }
        ],
        "name": "FundsDeposited",
        "type": "event"
      },
      {
        "anonymous": false,
        "inputs": [
          {
            "indexed": false,
            "internalType": "uint256",
            "name": "issueId",
            "type": "uint256"
          }
        ],
        "name": "IssueClosed",
        "type": "event"
      },
      {
        "inputs": [
          {
            "internalType": "uint256",
            "name": "issueId",
            "type": "uint256"
          },
          {
            "internalType": "uint256",
            "name": "bounty",
            "type": "uint256"
          }
        ],
        "name": "allocateBounty",
        "outputs": [],
        "stateMutability": "nonpayable",
        "type": "function"
      },
      {
        "inputs": [
          {
            "internalType": "uint256",
            "name": "issueId",
            "type": "uint256"
          }
        ],
        "name": "closeIssue",
        "outputs": [],
        "stateMutability": "nonpayable",
        "type": "function"
      },
      {
        "inputs": [],
        "name": "depositFunds",
        "outputs": [],
        "stateMutability": "payable",
        "type": "function"
      },
      {
        "inputs": [
          {
            "internalType": "uint256",
            "name": "",
            "type": "uint256"
          }
        ],
        "name": "issues",
        "outputs": [
          {
            "internalType": "uint256",
            "name": "bounty",
            "type": "uint256"
          },
          {
            "internalType": "bool",
            "name": "allocated",
            "type": "bool"
          },
          {
            "internalType": "address",
            "name": "funder",
            "type": "address"
          },
          {
            "internalType": "bool",
            "name": "closed",
            "type": "bool"
          }
        ],
        "stateMutability": "view",
        "type": "function"
      },
      {
        "stateMutability": "payable",
        "type": "receive"
      }
];

const validatorMultiSigABI = [
    {
        "inputs": [
          {
            "internalType": "address[]",
            "name": "_validators",
            "type": "address[]"
          },
          {
            "internalType": "uint256",
            "name": "_requiredConfirmations",
            "type": "uint256"
          }
        ],
        "stateMutability": "nonpayable",
        "type": "constructor"
      },
      {
        "anonymous": false,
        "inputs": [
          {
            "indexed": false,
            "internalType": "uint256",
            "name": "payoutId",
            "type": "uint256"
          },
          {
            "indexed": false,
            "internalType": "address",
            "name": "validator",
            "type": "address"
          }
        ],
        "name": "PaymentApproved",
        "type": "event"
      },
      {
        "inputs": [
          {
            "internalType": "uint256",
            "name": "payoutId",
            "type": "uint256"
          }
        ],
        "name": "approvePayment",
        "outputs": [],
        "stateMutability": "nonpayable",
        "type": "function"
      },
      {
        "inputs": [
          {
            "internalType": "uint256",
            "name": "",
            "type": "uint256"
          },
          {
            "internalType": "address",
            "name": "",
            "type": "address"
          }
        ],
        "name": "confirmations",
        "outputs": [
          {
            "internalType": "bool",
            "name": "",
            "type": "bool"
          }
        ],
        "stateMutability": "view",
        "type": "function"
      },
      {
        "inputs": [
          {
            "internalType": "uint256",
            "name": "payoutId",
            "type": "uint256"
          }
        ],
        "name": "isApproved",
        "outputs": [
          {
            "internalType": "bool",
            "name": "",
            "type": "bool"
          }
        ],
        "stateMutability": "view",
        "type": "function"
      },
      {
        "inputs": [],
        "name": "requiredConfirmations",
        "outputs": [
          {
            "internalType": "uint256",
            "name": "",
            "type": "uint256"
          }
        ],
        "stateMutability": "view",
        "type": "function"
      },
      {
        "inputs": [
          {
            "internalType": "uint256",
            "name": "",
            "type": "uint256"
          }
        ],
        "name": "validators",
        "outputs": [
          {
            "internalType": "address",
            "name": "",
            "type": "address"
          }
        ],
        "stateMutability": "view",
        "type": "function"
      }
];

const developerPayoutsABI = [
    {
        "inputs": [
          {
            "internalType": "address",
            "name": "_fundManager",
            "type": "address"
          },
          {
            "internalType": "address",
            "name": "_multiSig",
            "type": "address"
          }
        ],
        "stateMutability": "nonpayable",
        "type": "constructor"
      },
      {
        "inputs": [],
        "name": "ReentrancyGuardReentrantCall",
        "type": "error"
      },
      {
        "anonymous": false,
        "inputs": [
          {
            "indexed": false,
            "internalType": "uint256",
            "name": "payoutId",
            "type": "uint256"
          },
          {
            "indexed": false,
            "internalType": "uint256",
            "name": "issueId",
            "type": "uint256"
          },
          {
            "indexed": false,
            "internalType": "address",
            "name": "developer",
            "type": "address"
          }
        ],
        "name": "PayoutReleased",
        "type": "event"
      },
      {
        "anonymous": false,
        "inputs": [
          {
            "indexed": false,
            "internalType": "uint256",
            "name": "payoutId",
            "type": "uint256"
          },
          {
            "indexed": false,
            "internalType": "uint256",
            "name": "issueId",
            "type": "uint256"
          },
          {
            "indexed": false,
            "internalType": "address",
            "name": "developer",
            "type": "address"
          }
        ],
        "name": "PayoutRequested",
        "type": "event"
      },
      {
        "inputs": [],
        "name": "fundManager",
        "outputs": [
          {
            "internalType": "contract FundManager",
            "name": "",
            "type": "address"
          }
        ],
        "stateMutability": "view",
        "type": "function"
      },
      {
        "inputs": [],
        "name": "multiSig",
        "outputs": [
          {
            "internalType": "contract ValidatorMultiSig",
            "name": "",
            "type": "address"
          }
        ],
        "stateMutability": "view",
        "type": "function"
      },
      {
        "inputs": [],
        "name": "payoutCount",
        "outputs": [
          {
            "internalType": "uint256",
            "name": "",
            "type": "uint256"
          }
        ],
        "stateMutability": "view",
        "type": "function"
      },
      {
        "inputs": [
          {
            "internalType": "uint256",
            "name": "",
            "type": "uint256"
          }
        ],
        "name": "payouts",
        "outputs": [
          {
            "internalType": "uint256",
            "name": "issueId",
            "type": "uint256"
          },
          {
            "internalType": "address",
            "name": "developer",
            "type": "address"
          },
          {
            "internalType": "bool",
            "name": "paid",
            "type": "bool"
          }
        ],
        "stateMutability": "view",
        "type": "function"
      },
      {
        "inputs": [
          {
            "internalType": "uint256",
            "name": "payoutId",
            "type": "uint256"
          }
        ],
        "name": "releasePayout",
        "outputs": [],
        "stateMutability": "nonpayable",
        "type": "function"
      },
      {
        "inputs": [
          {
            "internalType": "uint256",
            "name": "issueId",
            "type": "uint256"
          }
        ],
        "name": "requestPayout",
        "outputs": [],
        "stateMutability": "nonpayable",
        "type": "function"
      },
      {
        "stateMutability": "payable",
        "type": "receive"
      }
];

// Connect to Ethereum
document.getElementById("connectButton").addEventListener("click", async () => {
    if (window.ethereum) {
        try {
            provider = new ethers.BrowserProvider(window.ethereum);

            signer = await provider.getSigner();
            fundManager = new ethers.Contract(fundManagerAddress, fundManagerABI, signer);
            validatorMultiSig = new ethers.Contract(validatorMultiSigAddress, validatorMultiSigABI, signer);
            developerPayouts = new ethers.Contract(developerPayoutsAddress, developerPayoutsABI, signer);

            // Log the connected wallet address
            const walletAddress = await signer.getAddress();
            console.log(`Wallet connected: ${walletAddress}`);

            alert("Wallet connected: " + walletAddress);
        } catch (error) {
            console.error("Connection error:", error);
            alert("Failed to connect MetaMask. Check the console for more details.");
        }
    } else {
        alert("Please install MetaMask!");
    }
});

// Allocate Bounty (Funder's functionality)
document.getElementById("allocateBounty").addEventListener("click", async () => {
    const issueId = document.getElementById("issueId").value;
    const bountyAmount = document.getElementById("bountyAmount").value;

    try {
        const tx = await fundManager.allocateBounty(issueId, ethers.parseUnits(bountyAmount, "ether"));
        console.log(`Transaction sent: ${tx.hash}`);
        await tx.wait();
        alert("Bounty allocated!");
    } catch (error) {
        console.error("Error allocating bounty:", error);
        alert("Error allocating bounty. Check the console for more details.");
    }
});

// View Bounties (Developer functionality)
document.getElementById("viewBounties").addEventListener("click", async () => {
    const issueId = document.getElementById("issueId").value;
    try {
        const issue = await fundManager.issues(issueId);
        
        if (issue.allocated) {
            alert(`Bounty for Issue #${issueId}: ${ethers.formatUnits(issue.bounty, "ether")} ETH`);
        } else {
            alert("No bounty allocated for this issue.");
        }
    } catch (error) {
        console.error("Error viewing bounties:", error);
        alert("Error viewing bounties. Check the console for more details.");
    }
});

// Request Payout (Developer functionality)
document.getElementById("requestPayout").addEventListener("click", async () => {
    const issueId = document.getElementById("issueId").value;

    try {
        const tx = await developerPayouts.requestPayout(issueId);
        console.log(`Transaction sent: ${tx.hash}`);
        await tx.wait();
        alert("Payout requested!");
    } catch (error) {
        console.error("Error requesting payout:", error);
        alert("Error requesting payout. Check the console for more details.");
    }
});

// Approve Payment (Validator functionality)
document.getElementById("approvePayment").addEventListener("click", async () => {
    const payoutId = 0; // For simplicity, assume payoutId 0 for now

    try {
        const tx = await validatorMultiSig.approvePayment(payoutId);
        console.log(`Transaction sent: ${tx.hash}`);
        await tx.wait();
        alert("Payment approved!");
    } catch (error) {
        console.error("Error approving payment:", error);
        alert("Error approving payment. Check the console for more details.");
    }
});
