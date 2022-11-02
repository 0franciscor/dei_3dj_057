import {Truck} from '../../../src/domain/truck/Truck';

import {expect} from 'chai';
import 'mocha';

describe("Create Truck", () => {
        
        it("truck should be created", async () => {
            const truck = Truck.create({
                autonomy: 1,
                capacity: 1,
                maxBatteryCapacity: 1,
                tare: 1,
                id: '',
                truckID: 'truckID',
                fastChargeTime: 1
            });
            expect(truck.getValue().autonomy.autonomy).to.equal(1);
            expect(truck.getValue().capacity.capacity).to.equal(1);
            expect(truck.getValue().maxBatteryCapacity.capacity).to.equal(1);
            expect(truck.getValue().tare.tare).to.equal(1);
            expect(truck.getValue().truckID.id).to.equal('truckID');
            expect(truck.getValue().fastChargeTime.time).to.equal(1);
        });
    
    });

describe ("Create an invalid truck", () => {
            
        it("truck should return error when autonomy <=0", async () => {
            const truck = Truck.create({
                autonomy: 0,
                capacity: 1,
                maxBatteryCapacity: 1,
                tare: 1,
                id: '',
                truckID: 'truckID',
                fastChargeTime: 1
            });
            expect(truck.isFailure).to.equal(true);

        });    

        it("truck should return error when capacity <=0", async () => {
            const truck = Truck.create({
                autonomy: 1,
                capacity: 0,
                maxBatteryCapacity: 1,
                tare: 1,
                id: '',
                truckID: 'truckID',
                fastChargeTime: 1
            });
            expect(truck.isFailure).to.equal(true);

        });

        it("truck should return error when maxBatteryCapacity <=0", async () => {
            const truck = Truck.create({
                autonomy: 1,
                capacity: 1,
                maxBatteryCapacity: 0,
                tare: 1,
                id: '',
                truckID: 'truckID',
                fastChargeTime: 1
            });
            expect(truck.isFailure).to.equal(true);

        });

        it("truck should return error when tare <=0", async () => {
            const truck = Truck.create({
                autonomy: 1,
                capacity: 1,
                maxBatteryCapacity: 1,
                tare: 0,
                id: '',
                truckID: 'truckID',
                fastChargeTime: 1
            });
            expect(truck.isFailure).to.equal(true);

        });

        it("truck should return error when truckID is invalid", async () => {
            const truck = Truck.create({
                autonomy: 1,
                capacity: 1,
                maxBatteryCapacity: 1,
                tare: 1,
                id: '',
                truckID: '',
                fastChargeTime: 1
            });
            expect(truck.isFailure).to.equal(true);

        });

        it("truck should return error when fastChargeTime is <=0", async () => {
            const truck = Truck.create({
                autonomy: 1,
                capacity: 1,
                maxBatteryCapacity: 1,
                tare: 1,
                id: '',
                truckID: 'truckID',
                fastChargeTime: 0
            });
            expect(truck.isFailure).to.equal(true);

        });
    
    });