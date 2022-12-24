import { expect } from "chai";
import 'mocha';
import { Document } from 'mongoose';
import "reflect-metadata";
import * as sinon from 'sinon';
import { Container } from 'typedi';
import { IPathPersistance } from "../../src/dataschema/IPathPersistance";
import { IPathDTO } from "../../src/dto/IPathDTO";
import { PathMap } from "../../src/mappers/PathMap";
import PathRepo from "../../src/repos/pathRepo";

describe('PathRepo Unit Tests', ()=>{
    const sandbox = sinon.createSandbox();
    beforeEach(()=> {
        Container.reset();

        let pathSchemaInstance = require('../../src/persistence/schemas/pathSchema').default;
        Container.set("pathSchema", pathSchemaInstance);
    });

    afterEach(()=>{
        sinon.restore();
        sandbox.restore();
    });

    it('Exists should return true', async()=>{

        const pathDTO = {
            pathID : 'path1',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        } as IPathDTO;

        const pathSchemaInstance = Container.get("pathSchema");

        sinon.stub(pathSchemaInstance,"findById").returns(true);
        const pathRepo = new PathRepo(pathSchemaInstance as any);
        const answer = await pathRepo.exists(PathMap.toDomain(pathDTO));
        expect(answer).to.be.true;
    })

    it('Save should return true', async()=>{

        const pathDTO = {
            domainId:"123",
            pathID : 'path1',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        } as IPathPersistance;

        const pathSchemaInstance = Container.get("pathSchema");
        const path = PathMap.toDomain(pathDTO);
        sinon.stub(pathSchemaInstance,"findOne").returns(null);
        sinon.stub(pathSchemaInstance,"create").returns(pathDTO as IPathPersistance);
        const pathRepo = new PathRepo(pathSchemaInstance as any);
        const answer = await pathRepo.save(path);
        expect(answer.pathID.id).to.equal(path.pathID.id);
        expect(answer.startWHId.startWHId).to.equal(path.startWHId.startWHId);
        expect(answer.destinationWHId.destinationWHId).to.equal(path.destinationWHId.destinationWHId);
        expect(answer.pathDistance.pathDistance).to.equal(path.pathDistance.pathDistance);
        expect(answer.pathTravelTime.pathTravelTime).to.equal(path.pathTravelTime.pathTravelTime);
        expect(answer.extraTravelTime.extraTravelTime).to.equal(path.extraTravelTime.extraTravelTime);
        expect(answer.wastedEnergy.wastedEnergy).to.equal(path.wastedEnergy.wastedEnergy);
    })


    it('Save should return path on edit', async()=>{

        const pathDTO = {
            domainId:"123",
            pathID : 'path1',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30,
            save(){ return this;   }
        } as IPathPersistance & Document<any,any,any>;
        
        const pathDTO2 = {
            domainId:"123",
            pathID : 'path1',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        } as IPathPersistance

        const pathSchemaInstance = Container.get("pathSchema");
        const path = PathMap.toDomain(pathDTO2);

        sinon.stub(pathSchemaInstance,"findOne").returns(pathDTO);
        const pathRepo = new PathRepo(pathSchemaInstance as any);
        const answer = await pathRepo.save(path);
        expect(answer.pathID.id).to.equal(path.pathID.id);
        expect(answer.startWHId.startWHId).to.equal(path.startWHId.startWHId);
        expect(answer.destinationWHId.destinationWHId).to.equal(path.destinationWHId.destinationWHId);
        expect(answer.pathDistance.pathDistance).to.equal(path.pathDistance.pathDistance);
        expect(answer.pathTravelTime.pathTravelTime).to.equal(path.pathTravelTime.pathTravelTime);
        expect(answer.extraTravelTime.extraTravelTime).to.equal(path.extraTravelTime.extraTravelTime);
        expect(answer.wastedEnergy.wastedEnergy).to.equal(path.wastedEnergy.wastedEnergy);
    });

    it ('Delete should return path on success', async() => {
        const pathDTO = {
            domainId:"123",
            pathID : 'path1',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        } as IPathPersistance & Document<any,any,any>;

        const pathSchemaInstance = Container.get("pathSchema");
        const path = PathMap.toDomain(pathDTO);
        sinon.stub(pathSchemaInstance,"findOne").returns(pathDTO);
        sinon.stub(pathSchemaInstance,"deleteOne").returns(pathDTO);
        const pathRepo = new PathRepo(pathSchemaInstance as any);
        const answer = await pathRepo.delete(path);
        expect(answer.pathID.id).to.equal(path.pathID.id);
        expect(answer.startWHId.startWHId).to.equal(path.startWHId.startWHId);
        expect(answer.destinationWHId.destinationWHId).to.equal(path.destinationWHId.destinationWHId);
        expect(answer.pathDistance.pathDistance).to.equal(path.pathDistance.pathDistance);
        expect(answer.pathTravelTime.pathTravelTime).to.equal(path.pathTravelTime.pathTravelTime);
        expect(answer.extraTravelTime.extraTravelTime).to.equal(path.extraTravelTime.extraTravelTime);
        expect(answer.wastedEnergy.wastedEnergy).to.equal(path.wastedEnergy.wastedEnergy);
    });

    it ('Delete should return path on fail', async() => {
        const pathDTO = {
            domainId:"123",
            pathID : 'path1',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        } as IPathPersistance & Document<any,any,any>;

        const pathSchemaInstance = Container.get("pathSchema");
        const path = PathMap.toDomain(pathDTO);

        sinon.stub(pathSchemaInstance,"findOne").returns(null);
        const pathRepo = new PathRepo(pathSchemaInstance as any);
        const answer = await pathRepo.delete(path);
        expect(answer.pathID.id).to.equal(path.pathID.id);
        expect(answer.startWHId.startWHId).to.equal(path.startWHId.startWHId);
        expect(answer.destinationWHId.destinationWHId).to.equal(path.destinationWHId.destinationWHId);
        expect(answer.pathDistance.pathDistance).to.equal(path.pathDistance.pathDistance);
        expect(answer.pathTravelTime.pathTravelTime).to.equal(path.pathTravelTime.pathTravelTime);
        expect(answer.extraTravelTime.extraTravelTime).to.equal(path.extraTravelTime.extraTravelTime);
        expect(answer.wastedEnergy.wastedEnergy).to.equal(path.wastedEnergy.wastedEnergy);
    });

    it('getPathById should return path on success', async()=>{
        const pathDTO = {
            domainId:"123",
            pathID : 'path1',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        } as IPathPersistance & Document<any,any,any>;

        const pathSchemaInstance = Container.get("pathSchema");
        const path = PathMap.toDomain(pathDTO);

        sinon.stub(pathSchemaInstance,"findOne").returns(pathDTO);
        const pathRepo = new PathRepo(pathSchemaInstance as any);
        const answer = await pathRepo.getPathById(pathDTO.pathID);
        expect(answer.pathID.id).to.equal(path.pathID.id);
        expect(answer.startWHId.startWHId).to.equal(path.startWHId.startWHId);
        expect(answer.destinationWHId.destinationWHId).to.equal(path.destinationWHId.destinationWHId);
        expect(answer.pathDistance.pathDistance).to.equal(path.pathDistance.pathDistance);
        expect(answer.pathTravelTime.pathTravelTime).to.equal(path.pathTravelTime.pathTravelTime);
        expect(answer.extraTravelTime.extraTravelTime).to.equal(path.extraTravelTime.extraTravelTime);
        expect(answer.wastedEnergy.wastedEnergy).to.equal(path.wastedEnergy.wastedEnergy);

    });

    it('getPathById should return path on fail', async()=>{
        const pathDTO = {
            domainId:"123",
            pathID : 'path1',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        } as IPathPersistance & Document<any,any,any>;

        const pathSchemaInstance = Container.get("pathSchema");
        const path = PathMap.toDomain(pathDTO);

        sinon.stub(pathSchemaInstance,"findOne").returns(null);
        const pathRepo = new PathRepo(pathSchemaInstance as any);
        const answer = await pathRepo.getPathById(pathDTO.pathID);
        expect(answer).to.equal(null);
    });

    it('getAllPaths should return a List ', async()=>{
        const pathDTO =[ {
            domainId:"123",
            pathID : 'path1',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        },{
            domainId:"1234",
            pathID : 'path12',
            startWHId : 'WH6',
            destinationWHId: 'WH5',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        }] as IPathPersistance[];

        let startWHId='';
        let destinationWHId= '';
        const pathSchemaInstance = Container.get("pathSchema");
        sinon.stub(pathSchemaInstance,"find").returns(pathDTO);
        const pathRepo = new PathRepo(pathSchemaInstance as any);
        const answer = await pathRepo.getAllPaths(startWHId,destinationWHId);
        expect(answer.length).to.equal(2);
    });


 })

