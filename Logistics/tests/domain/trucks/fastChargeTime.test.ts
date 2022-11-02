import {FastChargeTime} from '../../../src/domain/truck/FastChargeTime';

import {expect} from 'chai';
import 'mocha';

describe("Create FastChargeTime", () => {
    
    it("FastChargeTime should be 1", async () => {
        const fastChargeTime = FastChargeTime.create(1);
        expect(fastChargeTime.getValue().time).to.equal(1);
    });

});

describe ("Create an invalid FastChargeTime", () => {
        
    it("FastChargeTime should be null", async () => {
        const fastChargeTime = FastChargeTime.create(0);
        expect(fastChargeTime.error).to.equal("Fast charge time must be greater than 0");
    });

});