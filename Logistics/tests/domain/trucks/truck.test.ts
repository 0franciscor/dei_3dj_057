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
                truckID: 'gandaTruck',
                fastChargeTime: 1
            });
            expect(truck.getValue().autonomy.autonomy).to.equal(1);
            expect(truck.getValue().capacity.capacity).to.equal(1);
            expect(truck.getValue().maxBatteryCapacity.capacity).to.equal(1);
            expect(truck.getValue().tare.tare).to.equal(1);
            expect(truck.getValue().truckID.id).to.equal('gandaTruck');
            expect(truck.getValue().fastChargeTime.time).to.equal(1);
        });
    
    });

describe ("Create an invalid truck", () => {
            
        it("truck should be null", async () => {
            const truck = Truck.create({
                autonomy: NaN,
                capacity: NaN,
                maxBatteryCapacity: NaN,
                tare: NaN,
                id: '',
                truckID: '',
                fastChargeTime: NaN
            });
            
            expect(truck.isFailure).to.equal(true);

        });
    
    });