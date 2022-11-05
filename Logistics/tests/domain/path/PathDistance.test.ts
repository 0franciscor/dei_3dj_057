import { PathDistance } from "../../../src/domain/path/PathDistance";
import { expect } from "chai";
import "mocha";

describe("Create path distance",()=>{
    it("pathDistance should be 1", async ()=> {
        const pathDistance = PathDistance.create(1);
        expect(pathDistance.getValue().pathDistance).to.equal(1);
    });
});

describe ("Create invalid pathDistance", ()=> {
    it("pathDistance should return error", async () => {
        const pathDistance = PathDistance.create(-1);
        expect(pathDistance.error).to.equal("Not acceptable distance");
    });
});
