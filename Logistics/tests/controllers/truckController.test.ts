import {Response, Request, NextFunction} from 'express';
import { Container } from 'typedi';
import config from '../../config';
import { Result }  from '../../src/core/logic/Result';
import * as sinon from 'sinon';
import TruckController from '../../src/controllers/TruckController';
import ITruckService from '../../src/services/IServices/ITruckService';
import { ITruckDTO } from '../../src/dto/ITruckDTO';
import { describe } from 'node:test';
import 'mocha';


describe('TruckController', () => {

    it('createTruck', async () => {



    });


});