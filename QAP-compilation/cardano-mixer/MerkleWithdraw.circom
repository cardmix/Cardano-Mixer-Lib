include "mimcsponge.circom";
include "../circomlib/circuits/switcher.circom";
include "../circomlib/circuits/comparators.circom";

template Mixer(nTreeDepth) {
    signal input R;
    signal input A;
    signal input h;
    signal input hA;
    signal input curPos;
    signal input oldRewardHash;
    signal input newRewardHash;
    
    signal private input r1;
    signal private input r2;
    signal private input O[nTreeDepth];
    signal private input l[nTreeDepth];
    
    // Rewards account
    signal private input v1;
    signal private input v2;
    signal private input v3;
    
    signal output checkKey;
    signal output checkAddress;
    signal output checkRootPath;
    signal output checkOldHash;
    signal output checkNewHash;
    
    // Computing key
    component mimc_sponge_key = MiMCSponge(2, 220, 1);
    signal k1;
    mimc_sponge_key.ins[0] <== 0;
    mimc_sponge_key.ins[1] <== r1;
    mimc_sponge_key.k <== 0;
    mimc_sponge_key.outs[0] ==> k1;
    
    // Checking Key
    checkKey <== k1 - h;
        
    // Computing address hash
    component mimc_sponge_address = MiMCSponge(2, 220, 1);
    signal k2;
    mimc_sponge_address.ins[0] <== A;
    mimc_sponge_address.ins[1] <== r2;
    mimc_sponge_address.k <== 0;
    mimc_sponge_address.outs[0] ==> k2;
    
    // Checking Address
    checkAddress <== k2 - hA;
    
    // Computing leaf
    component mimc_sponge_tree[nTreeDepth+1];
    component switcher_tree[nTreeDepth];
    mimc_sponge_tree[0] = MiMCSponge(2, 220, 1);
    mimc_sponge_tree[0].k <== 0;
    mimc_sponge_tree[0].ins[0] <== r1;
    mimc_sponge_tree[0].ins[1] <== r2;
    
    // Computing path
    var i;
    for (i = 1; i <= nTreeDepth; i++) {
    	mimc_sponge_tree[i] = MiMCSponge(2, 220, 1);
	mimc_sponge_tree[i].k <== 0;
	if (i > 0) {
		switcher_tree[i-1] = Switcher();
		switcher_tree[i-1].L <== mimc_sponge_tree[i-1].outs[0];
		switcher_tree[i-1].R <== O[i-1];
		switcher_tree[i-1].sel <== l[i-1];
		mimc_sponge_tree[i].ins[0] <== switcher_tree[i-1].outL;
		mimc_sponge_tree[i].ins[1] <== switcher_tree[i-1].outR;
	}
    }
    
    // Computing mining reward
    signal m[nTreeDepth]
    m[0] <== l[0] + 1;
    for (i = 1; i < nTreeDepth; i++) {
    	m[i] <== m[i-1] + l[i] * (2 ** i);
    }
    
    // Checking root
    checkRootPath <== mimc_sponge_tree[nTreeDepth].outs[0] - R;
    
    // Computing old reward hash
    component old_reward_hash = MiMCSponge(2, 220, 1);
    signal k3;
    old_reward_hash.ins[0] <== v1;
    old_reward_hash.ins[1] <== v2;
    old_reward_hash.k <== 0;
    old_reward_hash.outs[0] ==> k3;
    
    // Checking old reward hash
    checkOldHash <== k3 - oldRewardHash;
    
    // Computing new reward hash
    component new_reward_hash = MiMCSponge(2, 220, 1);
    signal k4;
    new_reward_hash.ins[0] <== v1 + curPos - m[nTreeDepth-1];
    new_reward_hash.ins[1] <== v3;
    new_reward_hash.k <== 0;
    new_reward_hash.outs[0] ==> k4;
    
    // Checking new reward hash
    checkNewHash <== k4 - newRewardHash;
 }

 component main = Mixer(10);
