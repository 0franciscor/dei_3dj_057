import {Capacity} from '../../../src/domain/truck/Capacity';

import {expect} from 'chai';
import 'mocha';

describe("Create Capacity", () => {
    
    it("capacity should be 1", async () => {
        const capacity = Capacity.create(1);
        expect(capacity.getValue().capacity).to.equal(1);
    });

});

describe ("Create an invalid capacity", () => {
            
    it("capacity should be null", async () => {
        const capacity = Capacity.create(NaN);
        expect(capacity.error).to.equal("Capacity must be greater than 0");
    });

});