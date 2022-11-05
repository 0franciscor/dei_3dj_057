import PathService from '../../src//services/pathService'
import IPathService from '../../src/services/IServices/IPathService'
import { IPathDTO } from '../../src/dto/IPathDTO'
import * as sinon from 'sinon';
import Container from 'typedi';
import IPathRepo from '../../src/repos/IRepos/IPathRepo';
import { expect } from 'chai';
import { PathMap } from '../../src/mappers/PathMap';

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
        console.log(answer);
        expect(answer.errorValue()).to.equal("Path not found");
        
    });

})