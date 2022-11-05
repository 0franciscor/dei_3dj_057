import { WastedEnergy } from "../../../src/domain/path/WastedEnergy";
import { expect } from "chai";
import "mocha";

describe("Create path wasted Energy",()=>{
    it("WastedEnergy should be 1", async ()=> {
        const wastedEnergy = WastedEnergy.create(1);
        expect(wastedEnergy.getValue().wastedEnergy).to.equal(1);
    });
});

describe ("Create invalid WastedEnergy", ()=> {
    it("WastedEnergy should return error", async () => {
        const wastedEnergy = WastedEnergy.create(-1);
        expect(wastedEnergy.error).to.equal("This wasted energy is not acceptable");
    });

    it("WastedEnergy should return error", async () => {
        const wastedEnergy = WastedEnergy.create(0);
        expect(wastedEnergy.error).to.equal("This wasted energy is not acceptable");
    });
});
