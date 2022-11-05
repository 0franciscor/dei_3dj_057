import { PathID } from "../../../src/domain/path/PathID";
import { expect } from "chai";
import "mocha";

describe("Create path pathID",()=>{
    it("pathID should be 1", async ()=> {
        const pathID = PathID.create('1');
        expect(pathID.getValue().id).to.equal('1');
    });
});

describe ("Create invalid pathID", ()=> {
    it("ExtraTravelTime should return error", async () => {
        const pathID = PathID.create('');
        expect(pathID.error).to.equal("PathID must be greater than 0");
    });

});
