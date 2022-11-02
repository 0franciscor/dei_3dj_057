import {Tare} from '../../../src/domain/truck/Tare';

import {expect} from 'chai';
import 'mocha';

describe("Create Tare", () => {

    it("tare should be 1", async () => {
        const tare = Tare.create(1);
        expect(tare.getValue().tare).to.equal(1);
    });

});

describe ("Create an invalid tare", () => {

    it("tare should be null", async () => {
        const tare = Tare.create(NaN);
        expect(tare.error).to.equal("Tare must be greater than 0");
    });

});