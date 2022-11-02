import {MaxBatteryCapacity} from '../../../src/domain/truck/MaxBatteryCapacity';

import {expect} from 'chai';
import 'mocha';

describe("Create MaxBatteryCapacity", () => {

    it("maxBatteryCapacity should be 1", async () => {
        const maxBatteryCapacity = MaxBatteryCapacity.create(1);
        expect(maxBatteryCapacity.getValue().capacity).to.equal(1);
    });

});

describe ("Create an invalid MaxBatteryCapacity", () => {

    it("maxBatteryCapacity should be null", async () => {
        const maxBatteryCapacity = MaxBatteryCapacity.create(NaN);
        expect(maxBatteryCapacity.error).to.equal("Max battery capacity must be greater than 0");
    });

});
