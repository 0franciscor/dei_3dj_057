import {PackagingID} from '../../../src/domain/packaging/PackagingID';
import {expect} from "chai";
import "mocha";

describe("Create PackagingID", () => {
        
    it("packagingID should be 1", async () => {
        const packagingID = PackagingID.create("1");
        expect(packagingID.getValue().id).to.equal("1");
    });
    
});

describe ("Create an invalid PackagingID", () => {
    
    it("packagingID should return error", async () => {
        const packagingID = PackagingID.create("");
        expect(packagingID.error).to.equal("PackagingID length must be greater than 0");
    });

});
