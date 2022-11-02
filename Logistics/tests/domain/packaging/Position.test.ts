import {Position} from "../../../src/domain/packaging/Position";
import {expect} from "chai";
import "mocha";

describe("Create Position", () => {
    
    it("position should be 1", async () => {
        const position = Position.create(1);
        expect(position.getValue().Position).to.equal(1);
    });
    
});

describe ("Create an invalid Position", () => {

    it("position NaN should return error", async () => {
        const position = Position.create(NaN);
        expect(position.error).to.equal("Position must be greater than 0");
    });

    it("position 0 should return error", async () => {
        const position = Position.create(0);
        expect(position.error).to.equal("Position must be greater than 0");
    });

    it("position -1 should return error", async () => {
        const position = Position.create(-1);
        expect(position.error).to.equal("Position must be greater than 0");
    });

});