import {Autonomy} from '../../../src/domain/truck/Autonomy';

import {expect} from 'chai';
import 'mocha';

describe("Create Autonomy", () => {
    
    it("autonomy should be 1", async () => {
        const autonomy = Autonomy.create(1);
        expect(autonomy.getValue().autonomy).to.equal(1);
    });

});

describe ("Create an invalid autonomy", () => {
            
    it("autonomy should be null", async () => {
        const autonomy = Autonomy.create(NaN);
        expect(autonomy.error).to.equal("Autonomy must be greater than 0");
    });

});