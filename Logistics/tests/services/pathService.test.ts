import PathService from '../../src//services/pathService'
import IPathService from '../../src/services/IServices/IPathService'
import { IPathDTO } from '../../src/dto/IPathDTO'
import * as sinon from 'sinon';
import Container from 'typedi';
import IPathRepo from '../../src/repos/IRepos/IPathRepo';
import { expect } from 'chai';
import { PathMap } from '../../src/mappers/PathMap';
import {Path}  from '../../src/domain/path/Path'
import path from 'path';

describe('PathService Unit Tests', () =>{
    const sandbox = sinon.createSandbox();
    before(()=>{
        Container.reset();

        let pathSchemaInstance = require('../../src/persistence/schemas/pathSchema').default;
        Container.set("pathSchema",pathSchemaInstance);

        let pathRepoClass = require('../../src/repos/pathRepo').default;
        let pathRepoInstance = Container.get(pathRepoClass);
        Container.set("PathRepo",pathRepoInstance);
    });

    afterEach(() => {
        sinon.restore();
        sandbox.restore();
    });

    it ('getPath returns path + success', async () => {
        let body={
            id: 'id',
            pathID : 'path1',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        }

        let pathRepoInstance = Container.get("PathRepo");

        sinon.stub(pathRepoInstance,"getPathById").returns(PathMap.toDomain(body));
        const pathService = new PathService(pathRepoInstance as IPathRepo);
        const answer = await pathService.getPath(body.pathID);
        expect(answer.getValue().pathID).to.equal(body.pathID);
        expect(answer.getValue().startWHId).to.equal(body.startWHId);
        expect(answer.getValue().destinationWHId).to.equal(body.destinationWHId);
        expect(answer.getValue().pathDistance).to.equal(body.pathDistance);
        expect(answer.getValue().pathTravelTime).to.equal(body.pathTravelTime);
        expect(answer.getValue().extraTravelTime).to.equal(body.extraTravelTime);
        expect(answer.getValue().wastedEnergy).to.equal(body.wastedEnergy);
    });

    it ('getPath returns "path not found"', async () => {
        let body={
            id: '',
            pathID : 'pathID',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        }

        let pathRepoInstance = Container.get("PathRepo");

        sinon.stub(pathRepoInstance,"getPathById").returns(null);
        const pathService = new PathService(pathRepoInstance as IPathRepo);
        const answer = await pathService.getPath(body.pathID);
        expect(answer.errorValue()).to.equal("Path not found");
        
    });

    it ('createPath returns path + success', async() =>{
        let body={
            id: 'id',
            pathID : 'path1',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        };

        let pathRepoInstance = Container.get("PathRepo");

        sinon.stub(pathRepoInstance,"getPathById").returns(Promise.resolve(null));
        sinon.stub(pathRepoInstance,"save").returns(Promise.resolve(body));
        const pathService = new PathService(pathRepoInstance as IPathRepo);
        const answer = await pathService.createPath(body as IPathDTO);
        answer.getValue().id= "id";
        expect(answer.getValue().pathID).to.equal(body.pathID);
        expect(answer.getValue().startWHId).to.equal(body.startWHId);
        expect(answer.getValue().destinationWHId).to.equal(body.destinationWHId);
        expect(answer.getValue().pathDistance).to.equal(body.pathDistance);
        expect(answer.getValue().pathTravelTime).to.equal(body.pathTravelTime);
        expect(answer.getValue().extraTravelTime).to.equal(body.extraTravelTime);
        expect(answer.getValue().wastedEnergy).to.equal(body.wastedEnergy);
    });

    it('createPath returns "path already exists"', async() =>  {
        let body={
            id: 'id',
            pathID : 'path1',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        };  
        let pathRepoInstance = Container.get("PathRepo");
        sinon.stub(pathRepoInstance,"getPathById").returns(Promise.resolve(body));
        const pathService = new PathService(pathRepoInstance as IPathRepo);
        const answer = await pathService.createPath(body as IPathDTO);
        expect(answer.errorValue()).to.equal("Path already exists");
    });

    it('getAllPaths returns list', async()=>{
        let body=[{
            id: 'id1',
            pathID : 'path1',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        },{
            id: 'id2',
            pathID : 'path2',
            startWHId : 'WH5',
            destinationWHId: 'WH7',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        },];

        let body2 = {
            
        }

        let pathRepoInstance = Container.get("PathRepo");
        let paths: Path[] = [];
        body.forEach(path=>{
            paths.push(PathMap.toDomain(path));
        });
        sinon.stub(pathRepoInstance,"getAllPaths").returns(paths);
        const pathService = new PathService(pathRepoInstance as IPathRepo);
        const answer = await pathService.getAllPath(body2 as IPathDTO);

        expect(answer.getValue().length).to.equal(2);

    })

    it('getAllPaths by StartWHId returns list', async()=>{
        let body=[{
            id: 'id1',
            pathID : 'path1',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        },{
            id: 'id2',
            pathID : 'path2',
            startWHId : 'WH5',
            destinationWHId: 'WH7',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        },];

        let body2 = {
            startWHId : 'WH5'
        }

        let pathRepoInstance = Container.get("PathRepo");
        let paths: Path[] = [];
        body.forEach(path=>{
            paths.push(PathMap.toDomain(path));
        });
        sinon.stub(pathRepoInstance,"getAllPaths").returns(paths);
        const pathService = new PathService(pathRepoInstance as IPathRepo);
        const answer = await pathService.getAllPath(body2 as IPathDTO);

        expect(answer.getValue().length).to.equal(2);

    })

    it('getAllPaths by destinationWHId returns list', async()=>{
        let body=[{
            _id: 'id1',
            pathID : 'path1',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        },{
            _id: 'id2',
            pathID : 'path2',
            startWHId : 'WH5',
            destinationWHId: 'WH7',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        },];

        let body2 = {
            destinationWHId : 'WH6'
        }

        let pathRepoInstance = Container.get("PathRepo");
        let paths: Path[] = [];
        body.forEach(path=>{
            paths.push(PathMap.toDomain(path));
        });
        let listpaths: Path[] = [];
        listpaths.push(PathMap.toDomain(body[0]))
        
        sinon.stub(pathRepoInstance,"getAllPaths").returns(listpaths);
        const pathService = new PathService(pathRepoInstance as IPathRepo);
        const answer = await pathService.getAllPath(body2 as IPathDTO);

        expect(answer.getValue().length).to.equal(1);

    })

    
    it('getAllPaths by startWHId and destinationWHId returns list', async()=>{
        let body=[{
            id: 'id1',
            pathID : 'path1',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        },{
            id: 'id2',
            pathID : 'path2',
            startWHId : 'WH5',
            destinationWHId: 'WH7',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        },];

        let body2 = {
            startWHId : 'WH5',
            destinationWHId : 'WH6',

        }

        let pathRepoInstance = Container.get("PathRepo");
        let paths: Path[] = [];
        body.forEach(path=>{
            paths.push(PathMap.toDomain(path));
        });
        let listpaths: Path[] = [];
        listpaths.push(PathMap.toDomain(body[1]))
        
        sinon.stub(pathRepoInstance,"getAllPaths").returns(listpaths);
        const pathService = new PathService(pathRepoInstance as IPathRepo);
        const answer = await pathService.getAllPath(body2 as IPathDTO);

        expect(answer.getValue().length).to.equal(1);

    })

    it('update returns path', async() =>{
        let body={
            pathID : 'path1',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        };

        let pathRepoInstance = Container.get("PathRepo");

        sinon.stub(pathRepoInstance,"getPathById").returns(PathMap.toDomain(body));
        sinon.stub(pathRepoInstance,"save").returns(Promise.resolve(body));
        const pathService = new PathService(pathRepoInstance as IPathRepo);
        const answer = await pathService.updatePath(body as IPathDTO);
        expect(answer.getValue().pathID).to.equal(body.pathID);
        expect(answer.getValue().startWHId).to.equal(body.startWHId);
        expect(answer.getValue().destinationWHId).to.equal(body.destinationWHId);
        expect(answer.getValue().pathDistance).to.equal(body.pathDistance);
        expect(answer.getValue().pathTravelTime).to.equal(body.pathTravelTime);
        expect(answer.getValue().extraTravelTime).to.equal(body.extraTravelTime);
        expect(answer.getValue().wastedEnergy).to.equal(body.wastedEnergy);
    });

    it('update returns path not found', async() =>{
        let body={
            id: 'id1',
            pathID : 'path1',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        };

        let pathRepoInstance = Container.get("PathRepo");

        sinon.stub(pathRepoInstance,"getPathById").returns(Promise.resolve(null));
        const pathService = new PathService(pathRepoInstance as IPathRepo);
        const answer = await pathService.updatePath(body as IPathDTO);

        expect(answer.errorValue()).to.equal("Path not found");
    });

    it('deletePath returns path', async()=>{
        let body={
            id: 'id1',
            pathID : 'path1',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        };

        let pathRepoInstance= Container.get("PathRepo");

        sinon.stub(pathRepoInstance, "getPathById").returns(PathMap.toDomain(body));
        sinon.stub(pathRepoInstance, "delete").returns(PathMap.toDomain(body));
        const pathService = new PathService(pathRepoInstance as IPathRepo);
        const answer = await pathService.deletePath(body.pathID);
        expect(answer.getValue().pathID).to.equal(body.pathID);
        expect(answer.getValue().startWHId).to.equal(body.startWHId);
        expect(answer.getValue().destinationWHId).to.equal(body.destinationWHId);
        expect(answer.getValue().pathDistance).to.equal(body.pathDistance);
        expect(answer.getValue().pathTravelTime).to.equal(body.pathTravelTime);
        expect(answer.getValue().extraTravelTime).to.equal(body.extraTravelTime);
        expect(answer.getValue().wastedEnergy).to.equal(body.wastedEnergy);

    });

    it('deletePath returns path not found', async()=>{
        let body={
            id: 'id1',
            pathID : 'path1',
            startWHId : 'WH5',
            destinationWHId: 'WH6',
            pathDistance: 200,
            pathTravelTime: 20,
            extraTravelTime: 0,
            wastedEnergy: 30
        };

        let pathRepoInstance= Container.get("PathRepo");

        sinon.stub(pathRepoInstance, "getPathById").returns(null);
        const pathService = new PathService(pathRepoInstance as IPathRepo);
        const answer = await pathService.deletePath(body.pathID);
        expect(answer.errorValue()).to.equal("Path not found");

    });
})