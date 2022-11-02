import {DeliveryID} from "../../../src/domain/packaging/DeliveryID";
import {expect} from "chai";
import "mocha";

describe("Create DeliveryID", () => {
    
    it("deliveryID should be 1", async () => {
        const deliveryID = DeliveryID.create("1");
        expect(deliveryID.getValue().id).to.equal("1");
    });
    
});

describe ("Create an invalid DeliveryID", () => {

    it("deliveryID should return error", async () => {
        const deliveryID = DeliveryID.create("");
        expect(deliveryID.error).to.equal("DeliveryID length must be greater than 0");
    });

});