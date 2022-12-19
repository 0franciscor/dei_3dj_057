import { expect } from "chai";
import 'mocha';
import { Document } from 'mongoose';
import "reflect-metadata";
import * as sinon from 'sinon';
import { Container } from 'typedi';
import { ITruckPersistence } from "../../src/dataschema/ITruckPersistence";
import { ITruckDTO } from '../../src/dto/ITruckDTO';
import { TruckMap } from "../../src/mappers/TruckMap";
import TruckRepo from "../../src/repos/truckRepo";

describe('TruckRepo Unit Tests', () => {

    const sandbox = sinon.createSandbox();
    beforeEach(() => {
        Container.reset();

        let truckSchemaInstance = require('../../src/persistence/schemas/truckSchema').default;
        Container.set("truckSchema", truckSchemaInstance);

    });
    
    afterEach(() => {
        sinon.restore();
        sandbox.restore();
    });

    it('Exists should return true', async () => {

    
        const truckDTO = {
            truckID: "123",
            autonomy: 100,
            capacity: 100,
            fastChargeTime: 100,
            maxBatteryCapacity: 100,
            tare: 100
        } as ITruckDTO;

        const truckSchemaInstance = Container.get("truckSchema");

        sinon.stub(truckSchemaInstance, "findById").returns(true);
        const truckRepo = new TruckRepo(truckSchemaInstance as any);
        const answer = await truckRepo.exists(TruckMap.toDomain(truckDTO));
        expect(answer).to.be.true;
    });

    it('Save should return truck', async () => {

    
        const truckDTO = {
            domainId: "123",
            truckID: "123",
            autonomy: 100,
            capacity: 100,
            fastChargeTime: 100,
            maxBatteryCapacity: 100,
            tare: 100
        } as ITruckPersistence;

        const truckSchemaInstance = Container.get("truckSchema");
        const truck = TruckMap.toDomain(truckDTO);
        sinon.stub(truckSchemaInstance, "findOne").returns(null);
        sinon.stub(truckSchemaInstance, "create").returns(truckDTO as ITruckPersistence);
        const truckRepo = new TruckRepo(truckSchemaInstance as any);
        const answer = await truckRepo.save(truck);
        expect(answer.truckID.id).to.equal(truck.truckID.id);
        expect(answer.tare.tare).to.equal(truck.tare.tare);
        expect(answer.capacity.capacity).to.equal(truck.capacity.capacity);
        expect(answer.maxBatteryCapacity.capacity).to.equal(truck.maxBatteryCapacity.capacity);
        expect(answer.autonomy.autonomy).to.equal(truck.autonomy.autonomy);
        expect(answer.fastChargeTime.time).to.equal(truck.fastChargeTime.time);

    });

    it('Save should return truck on edit', async () => {

    
        const truckDTO = {
            domainId: "123",
            truckID: "123",
            autonomy: 100,
            capacity: 100,
            fastChargeTime: 100,
            maxBatteryCapacity: 100,
            tare: 100,
            save() { return this; }
        } as ITruckPersistence & Document<any, any, any>;

        const truckDTO2 = {
            domainId: "123",
            truckID: "123",
            autonomy: 200,
            capacity: 100,
            fastChargeTime: 100,
            maxBatteryCapacity: 100,
            tare: 100
        } as ITruckPersistence;

        const truckSchemaInstance = Container.get("truckSchema");
        const truck = TruckMap.toDomain(truckDTO2);
       
        sinon.stub(truckSchemaInstance, "findOne").returns(truckDTO);
        const truckRepo = new TruckRepo(truckSchemaInstance as any);
        const answer = await truckRepo.save(truck);
        expect(answer.truckID.id).to.equal(truck.truckID.id);
        expect(answer.tare.tare).to.equal(truck.tare.tare);
        expect(answer.capacity.capacity).to.equal(truck.capacity.capacity);
        expect(answer.maxBatteryCapacity.capacity).to.equal(truck.maxBatteryCapacity.capacity);
        expect(answer.autonomy.autonomy).to.equal(truck.autonomy.autonomy);
        expect(answer.fastChargeTime.time).to.equal(truck.fastChargeTime.time);

    });

    it('Delete should return truck on success', async () => {

    
        const truckDTO = {
            domainId: "123",
            truckID: "123",
            autonomy: 100,
            capacity: 100,
            fastChargeTime: 100,
            maxBatteryCapacity: 100,
            tare: 100,
            save() { return this; }
        } as ITruckPersistence & Document<any, any, any>;

        const truckSchemaInstance = Container.get("truckSchema");
        const truck = TruckMap.toDomain(truckDTO);
    
        sinon.stub(truckSchemaInstance, "findOne").returns(truckDTO);
        sinon.stub(truckSchemaInstance, "deleteOne").returns(truckDTO);
        const truckRepo = new TruckRepo(truckSchemaInstance as any);
        const answer = await truckRepo.delete(truck);
        expect(answer.truckID.id).to.equal(truck.truckID.id);
        expect(answer.tare.tare).to.equal(truck.tare.tare);
        expect(answer.capacity.capacity).to.equal(truck.capacity.capacity);
        expect(answer.maxBatteryCapacity.capacity).to.equal(truck.maxBatteryCapacity.capacity);
        expect(answer.autonomy.autonomy).to.equal(truck.autonomy.autonomy);
        expect(answer.fastChargeTime.time).to.equal(truck.fastChargeTime.time);

    });


    it('Delete should return truck on fail', async () => {

    
        const truckDTO = {
            domainId: "123",
            truckID: "123",
            autonomy: 100,
            capacity: 100,
            fastChargeTime: 100,
            maxBatteryCapacity: 100,
            tare: 100,
            save() { return this; }
        } as ITruckPersistence & Document<any, any, any>;


        const truckSchemaInstance = Container.get("truckSchema");
        const truck = TruckMap.toDomain(truckDTO);
       
        sinon.stub(truckSchemaInstance, "findOne").returns(null);
        const truckRepo = new TruckRepo(truckSchemaInstance as any);
        const answer = await truckRepo.delete(truck);
        expect(answer.truckID.id).to.equal(truck.truckID.id);
        expect(answer.tare.tare).to.equal(truck.tare.tare);
        expect(answer.capacity.capacity).to.equal(truck.capacity.capacity);
        expect(answer.maxBatteryCapacity.capacity).to.equal(truck.maxBatteryCapacity.capacity);
        expect(answer.autonomy.autonomy).to.equal(truck.autonomy.autonomy);
        expect(answer.fastChargeTime.time).to.equal(truck.fastChargeTime.time);

    });
    
    
    it('getTruckById should return truck on success', async () => {

    
        const truckDTO = {
            domainId: "123",
            truckID: "123",
            autonomy: 100,
            capacity: 100,
            fastChargeTime: 100,
            maxBatteryCapacity: 100,
            tare: 100,
            save() { return this; }
        } as ITruckPersistence & Document<any, any, any>;

        const truckSchemaInstance = Container.get("truckSchema");
        const truck = TruckMap.toDomain(truckDTO);
    
        sinon.stub(truckSchemaInstance, "findOne").returns(truckDTO);
        const truckRepo = new TruckRepo(truckSchemaInstance as any);
        const answer = await truckRepo.getTruckById(truckDTO.truckID);
        expect(answer.truckID.id).to.equal(truck.truckID.id);
        expect(answer.tare.tare).to.equal(truck.tare.tare);
        expect(answer.capacity.capacity).to.equal(truck.capacity.capacity);
        expect(answer.maxBatteryCapacity.capacity).to.equal(truck.maxBatteryCapacity.capacity);
        expect(answer.autonomy.autonomy).to.equal(truck.autonomy.autonomy);
        expect(answer.fastChargeTime.time).to.equal(truck.fastChargeTime.time);

    });


    it('getTruckById should return null on fail', async () => {

    
        const truckDTO = {
            domainId: "123",
            truckID: "123",
            autonomy: 100,
            capacity: 100,
            fastChargeTime: 100,
            maxBatteryCapacity: 100,
            tare: 100,
            save() { return this; }
        } as ITruckPersistence & Document<any, any, any>;


        const truckSchemaInstance = Container.get("truckSchema");
        const truck = TruckMap.toDomain(truckDTO);
       
        sinon.stub(truckSchemaInstance, "findOne").returns(null);
        const truckRepo = new TruckRepo(truckSchemaInstance as any);
        const answer = await truckRepo.getTruckById(truckDTO.truckID);
        expect(answer).to.equal(null);


    });

    it('getAllTrucks should return a list', async () => {

    
        const truckDTO = [{
            domainId: "123",
            truckID: "123",
            autonomy: 100,
            capacity: 100,
            fastChargeTime: 100,
            maxBatteryCapacity: 100,
            tare: 100
        },
        {
            domainId: "1234",
            truckID: "1234",
            autonomy: 100,
            capacity: 100,
            fastChargeTime: 100,
            maxBatteryCapacity: 100,
            tare: 100
        }] as ITruckPersistence[];



        const truckSchemaInstance = Container.get("truckSchema");
       
        sinon.stub(truckSchemaInstance, "find").returns(truckDTO);
        const truckRepo = new TruckRepo(truckSchemaInstance as any);
        const answer = await truckRepo.getAllTrucks();
        expect(answer.length).to.equal(2);


    });



    

});
