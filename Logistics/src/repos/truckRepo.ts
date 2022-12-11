import { Service, Inject } from 'typedi';

import ITruckRepo from './IRepos/ITruckRepo';
import { Truck } from '../domain/truck/Truck';
import { ITruckPersistence } from '../dataschema/ITruckPersistence';
import { TruckMap } from '../mappers/TruckMap';

import { Document, FilterQuery, Model } from 'mongoose';
import { TruckID } from '../domain/truck/TruckID';
import { raw } from 'body-parser';


@Service()
export default class TruckRepo implements ITruckRepo {
    
    constructor(
        @Inject('truckSchema') private truckSchema : Model<ITruckPersistence & Document>,
    ) { }

    public async exists(truck: Truck): Promise<boolean> {
        const idX = truck.id instanceof TruckID ? (<TruckID>truck.id) : truck.id;

        const query = { domainId: idX}; 
        const truckDocument = await this.truckSchema.findById( query as FilterQuery<ITruckPersistence & Document>);

        return !!truckDocument === true;
    }

    public async save(truck: Truck): Promise<Truck> {
        const query = { truckID: truck.truckID.id};
        const truckDocument = await this.truckSchema.findOne( query as FilterQuery<ITruckPersistence & Document> );
        try {
            
            if(truckDocument === null) {
                const rawTruck: any = TruckMap.toPersistence(truck);
                const truckCreated = await this.truckSchema.create(rawTruck);
                return TruckMap.toDomain(truckCreated);
            }
            else{
                truckDocument.truckID = truck.truckID.id;
                truckDocument.autonomy = truck.autonomy.autonomy;
                truckDocument.tare = truck.tare.tare;
                truckDocument.capacity = truck.capacity.capacity;
                truckDocument.maxBatteryCapacity = truck.maxBatteryCapacity.capacity;
                truckDocument.fastChargeTime = truck.fastChargeTime.time;
                truckDocument.active = truck.active.active;
                await truckDocument.save();
                return truck;
            }

        } catch (error) {
            throw error;
        }

    }

    public async delete(truck: Truck): Promise<Truck> {
        const query = { truckID: truck.truckID.id};
        const truckDocument = await this.truckSchema.findOne( query as FilterQuery<ITruckPersistence & Document> );
        
        try {
            if(truckDocument === null) {
                return truck;
            }
            else{
                await this.truckSchema.deleteOne( query as FilterQuery<ITruckPersistence & Document> );
                return truck;
            }

        } catch (error) {
            throw error;
        }
    }

    public async getTruckById(id: string): Promise<Truck> {
        const query = { truckID: id};
        const truckDocument = await this.truckSchema.findOne( query as FilterQuery<ITruckPersistence & Document> );

        if(truckDocument === null) {
            return null;
        }
        else{
            return TruckMap.toDomain(truckDocument);
        }
    }
    public async getAllTrucks(): Promise<Truck[]> {
        const trucksDocument = await this.truckSchema.find();
        let trucks: Truck[] = [];
        trucksDocument.forEach(truck => {
            trucks.push(TruckMap.toDomain(truck));
        });
        return trucks;
    }



}