import {ZPosition} from "../../../src/domain/packaging/ZPosition";
import {expect} from "chai";
import "mocha";

describe("Create Position Z", () => {
    
    it("zPosition should be 1", async () => {
        const zPosition = ZPosition.create(1);
        expect(zPosition.getValue().ZPosition).to.equal(1);
    });
    
});

describe ("Create an invalid zPosition", () => {

    it("position NaN should return error", async () => {
        const zPosition = ZPosition.create(NaN);
        expect(zPosition.error).to.equal("Z Position must be greater than 0");
    });

    it("zPosition 0 should return error", async () => {
        const zPosition = ZPosition.create(0);
        expect(zPosition.error).to.equal("Z Position must be greater than 0");
    });

    it("zPosition 8 should return error", async () => {
        const zPosition = ZPosition.create(8);
        expect(zPosition.error).to.equal("Z Position must be less than 8");
    });

});