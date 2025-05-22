// scripts/deploy.js
async function main() {
    // Get available accounts
    const [deployer, addr1, addr2, addr3] = await ethers.getSigners();
  
    const deployerAddress = await deployer.getAddress();
    console.log("Deploying contracts with account:", deployerAddress);
  
    // Get deployer's balance via the provider
    const deployerBalance = await ethers.provider.getBalance(deployerAddress);
    console.log("Account balance:", ethers.formatEther(deployerBalance));
  
    // Deploy FundManager contract
    const FundManager = await ethers.getContractFactory("FundManager");
    const fundManager = await FundManager.deploy();
    await fundManager.waitForDeployment();
    console.log("FundManager deployed to:", fundManager.target);
  
    // Deploy ValidatorMultiSig contract with three validators and 2 required confirmations
    const ValidatorMultiSig = await ethers.getContractFactory("ValidatorMultiSig");
    const validators = [
      await addr1.getAddress(),
      await addr2.getAddress(),
      await addr3.getAddress()
    ];
    const validatorMultiSig = await ValidatorMultiSig.deploy(validators, 2);
    await validatorMultiSig.waitForDeployment();
    console.log("ValidatorMultiSig deployed to:", validatorMultiSig.target);
  
    // Deploy DeveloperPayouts contract using addresses of FundManager and ValidatorMultiSig
    const DeveloperPayouts = await ethers.getContractFactory("DeveloperPayouts");
    const developerPayouts = await DeveloperPayouts.deploy(fundManager.target, validatorMultiSig.target);
    await developerPayouts.waitForDeployment();
    console.log("DeveloperPayouts deployed to:", developerPayouts.target);
  }
  
  main()
    .then(() => process.exit(0))
    .catch((error) => {
      console.error(error);
      process.exit(1);
    });
  