import {Packaging} from '../../../src/domain/packaging/Packaging';
import {expect} from "chai";
import "mocha";

describe("Create Packaging", () => {
        
    it("packaging should be created", async () => {
        
        const packaging = Packaging.create({
            id: '',
            deliveryID: 'gandaDelivery',
            packagingID: 'gandaPackaging',
            truckID: 'gandaTruck',
            xPosition: 1,
            yPosition: 1,
            zPosition: 1
        });
        expect(packaging.getValue()).to.equal(packaging.getValue());
        expect(packaging.getValue().deliveryID.id).to.equal('gandaDelivery');
        expect(packaging.getValue().packagingID.id).to.equal('gandaPackaging');
        expect(packaging.getValue().truckID.id).to.equal('gandaTruck');
        expect(packaging.getValue().xPosition.XPosition).to.equal(1);
        expect(packaging.getValue().yPosition.YPosition).to.equal(1);
        expect(packaging.getValue().zPosition.ZPosition).to.equal(1);

    });
    
});

describe ("Create an invalid Packaging", () => {
        
    it("packaging should return error when deliveryID is invalid", async () => {
        const packaging = Packaging.create({
            id: '',
            deliveryID: '',
            packagingID: 'packagingID',
            truckID: 'truckID',
            xPosition: 1,
            yPosition: 1,
            zPosition: 1
        });
        expect(packaging.isFailure).to.equal(true);

    });   

    it("packaging should return error when packagingID is invalid", async () => {
        const packaging = Packaging.create({
            id: '',
            deliveryID: 'deliveryID',
            packagingID: '',
            truckID: 'truckID',
            xPosition: 1,
            yPosition: 1,
            zPosition: 1
        });
        expect(packaging.isFailure).to.equal(true);

    });

    it("packaging should return error when truckID is invalid", async () => {
        const packaging = Packaging.create({
            id: '',
            deliveryID: 'deliveryID',
            packagingID: "packagingID",
            truckID: '',
            xPosition: 1,
            yPosition: 1,
            zPosition: 1
        });
        expect(packaging.isFailure).to.equal(true);

    });

    it("packaging should return error when xPosition is <=0", async () => {
        const packaging = Packaging.create({
            id: '',
            deliveryID: 'deliveryID',
            packagingID: "packagingID",
            truckID: 'truckID',
            xPosition: 0,
            yPosition: 1,
            zPosition: 1
        });
        expect(packaging.isFailure).to.equal(true);

    });

    it("packaging should return error when xPosition is >=10", async () => {
        const packaging = Packaging.create({
            id: '',
            deliveryID: 'deliveryID',
            packagingID: "packagingID",
            truckID: 'truckID',
            xPosition: 10,
            yPosition: 1,
            zPosition: 1
        });
        expect(packaging.isFailure).to.equal(true);

    });


    it("packaging should return error when yPosition is <=0", async () => {
        const packaging = Packaging.create({
            id: '',
            deliveryID: 'deliveryID',
            packagingID: "packagingID",
            truckID: 'truckID',
            xPosition: 1,
            yPosition: 0,
            zPosition: 1
        });
        expect(packaging.isFailure).to.equal(true);

    });

    it("packaging should return error when yPosition is >=20", async () => {
        const packaging = Packaging.create({
            id: '',
            deliveryID: 'deliveryID',
            packagingID: "packagingID",
            truckID: 'truckID',
            xPosition: 1,
            yPosition: 20,
            zPosition: 1
        });
        expect(packaging.isFailure).to.equal(true);

    });

    it("packaging should return error when zPosition is <=0", async () => {
        const packaging = Packaging.create({
            id: '',
            deliveryID: 'deliveryID',
            packagingID: "packagingID",
            truckID: 'truckID',
            xPosition: 1,
            yPosition: 1,
            zPosition: 0
        });
        expect(packaging.isFailure).to.equal(true);

    });

    it("packaging should return error when zPosition is >=8", async () => {
        const packaging = Packaging.create({
            id: '',
            deliveryID: 'deliveryID',
            packagingID: "packagingID",
            truckID: 'truckID',
            xPosition: 1,
            yPosition: 1,
            zPosition: 8
        });
        expect(packaging.isFailure).to.equal(true);

    });
    


});