import {YPosition} from "../../../src/domain/packaging/YPosition";
import {expect} from "chai";
import "mocha";

describe("Create Position Y", () => {
    
    it("yPosition should be 1", async () => {
        const yPosition = YPosition.create(1);
        expect(yPosition.getValue().YPosition).to.equal(1);
    });
    
});

describe ("Create an invalid YPosition", () => {

    it("yPosition NaN should return error", async () => {
        const yPosition = YPosition.create(NaN);
        expect(yPosition.error).to.equal("Y Position must be greater than 0");
    });

    it("yPosition 0 should return error", async () => {
        const yPosition = YPosition.create(0);
        expect(yPosition.error).to.equal("Y Position must be greater than 0");
    });

    it("yPosition 10 should return error", async () => {
        const yPosition = YPosition.create(20);
        expect(yPosition.error).to.equal("Y Position must be less than 20");
    });

});