const { expect } = require("chai");
const { ethers } = require("hardhat");

describe("Decentralized Funding System", function () {
  let fundManager, validatorMultiSig, developerPayouts;
  let owner, addr1, addr2, validator1, validator2, validator3;

  beforeEach(async function () {
    [owner, addr1, addr2, validator1, validator2, validator3] = await ethers.getSigners();

    // Deploy FundManager
    const FundManager = await ethers.getContractFactory("FundManager");
    fundManager = await FundManager.deploy();
    console.log("FundManager deployed at:", fundManager.address); // Log FundManager address

    // Deploy ValidatorMultiSig
    const ValidatorMultiSig = await ethers.getContractFactory("ValidatorMultiSig");
    validatorMultiSig = await ValidatorMultiSig.deploy(
      [validator1.address, validator2.address, validator3.address],
      2
    );
    console.log("ValidatorMultiSig deployed at:", validatorMultiSig.address); // Log ValidatorMultiSig address

    // Deploy DeveloperPayouts
    const DeveloperPayouts = await ethers.getContractFactory("DeveloperPayouts");
    developerPayouts = await DeveloperPayouts.deploy(await fundManager.getAddress(), await validatorMultiSig.getAddress());
    console.log("DeveloperPayouts deployed at:", developerPayouts.address); // Log DeveloperPayouts address

    // FundManager receives a deposit.
    await fundManager.depositFunds({ value: ethers.parseEther("5") });

    // Manually fund DeveloperPayouts contract (to prevent balance issues)
    await owner.sendTransaction({
      to: await developerPayouts.getAddress(),
      value: ethers.parseEther("5"),
    });
  });

  it("Should allocate bounty and close issue", async function () {
    await fundManager.allocateBounty(1, ethers.parseEther("1"));
    let issue = await fundManager.issues(1);
    expect(issue.bounty).to.equal(ethers.parseEther("1"));

    await fundManager.closeIssue(1);
    issue = await fundManager.issues(1);
    expect(issue.closed).to.be.true;
  });

  it("Should allow developer to request and release payout after validator approval", async function () {
    await fundManager.allocateBounty(2, ethers.parseEther("1"));
    await fundManager.closeIssue(2);

    await developerPayouts.connect(addr2).requestPayout(2);

    await validatorMultiSig.connect(validator1).approvePayment(0);
    await validatorMultiSig.connect(validator2).approvePayment(0);

    console.log("Validator approvals:", await validatorMultiSig.isApproved(0));
    console.log("Contract balance before:", ethers.formatEther(await ethers.provider.getBalance(developerPayouts.getAddress())));

    const balanceBefore = await ethers.provider.getBalance(addr2.address);
    
    await developerPayouts.releasePayout(0);

    const payout = await developerPayouts.payouts(0);
    expect(payout.paid).to.be.true;

    const balanceAfter = await ethers.provider.getBalance(addr2.address);
    expect(balanceAfter).to.be.above(balanceBefore);

    console.log("Contract balance after:", ethers.formatEther(await ethers.provider.getBalance(developerPayouts.getAddress())));
  });
});
