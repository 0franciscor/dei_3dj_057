import { PathTravelTime } from "../../../src/domain/path/PathTravelTime";
import { expect } from "chai";
import "mocha";

describe("Create path travel time",()=>{
    it("pathTravelTime should be 1", async ()=> {
        const pathTravelTime = PathTravelTime.create(1);
        expect(pathTravelTime.getValue().pathTravelTime).to.equal(1);
    });
});

describe ("Create invalid pathTravelTime", ()=> {
    it("pathTravelTime should return error", async () => {
        const pathTravelTime = PathTravelTime.create(-1);
        expect(pathTravelTime.error).to.equal("This travel time is not acceptable");
    });

    it("pathTravelTime should return error", async () => {
        const pathTravelTime = PathTravelTime.create(0);
        expect(pathTravelTime.error).to.equal("This travel time is not acceptable");
    });
});
