import {TruckID} from '../../../src/domain/truck/TruckID';

import {expect} from 'chai';
import 'mocha';

describe("Create TruckID", () => {

    it("truckID should be 1", async () => {
        const truckID = TruckID.create("1");
        expect(truckID.getValue().id).to.equal("1");
    });

});

describe ("Create an invalid TruckID", () => {

    it("truckID should be null", async () => {
        const truckID = TruckID.create("");
        expect(truckID.error).to.equal("TruckID must be greater than 0");
    });

});