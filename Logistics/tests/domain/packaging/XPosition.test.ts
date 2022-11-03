import {XPosition} from "../../../src/domain/packaging/XPosition";
import {expect} from "chai";
import "mocha";

describe("Create Position X", () => {
    
    it("xPosition should be 1", async () => {
        const xPosition = XPosition.create(1);
        expect(xPosition.getValue().XPosition).to.equal(1);
    });
    
});

describe ("Create an invalid XPosition", () => {

    it("xPosition NaN should return error", async () => {
        const xPosition = XPosition.create(NaN);
        expect(xPosition.error).to.equal("X Position must be greater than 0");
    });

    it("xPosition 0 should return error", async () => {
        const xPosition = XPosition.create(0);
        expect(xPosition.error).to.equal("X Position must be greater than 0");
    });

    it("xPosition 10 should return error", async () => {
        const xPosition = XPosition.create(10);
        expect(xPosition.error).to.equal("X Position must be less than 10");
    });

});