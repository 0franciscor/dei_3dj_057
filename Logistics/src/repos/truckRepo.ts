import { Service, Inject } from 'typedi';

import ITruckRepo from '../services/IRepos/ITruckRepo';
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
        const idX = truck.id instanceof TruckID ? (<TruckID>truck.id).toValue : truck.id;

        const query = { domainId: idX}; 
        const truckDocument = await this.truckSchema.findOne( query as FilterQuery<ITruckPersistence & Document>);

        return !!truckDocument === true;
    }

    public async save(truck: Truck): Promise<Truck> {
        const query = { domainId: truck.id.toString()};
        
        const truckDocument = await this.truckSchema.findOne( query );
        try {
            
            if(truckDocument === null) {
                console.log("Truck not found, creating new one");
                const rawTruck: any = TruckMap.toPersistence(truck);
                console.log(rawTruck)
                const truckCreated = await this.truckSchema.create(rawTruck);
                console.log(truckCreated);
                return TruckMap.toDomain(truckCreated);
            }
            else{
                truckDocument.autonomy = truck.autonomy.autonomy;
                truckDocument.tare = truck.tare.tare;
                truckDocument.capacity = truck.capacity.capacity;
                truckDocument.maxBatteryCapacity = truck.maxBatteryCapacity.capacity;
                truckDocument.fastChargeTime = truck.fastChargeTime.time;
                await truckDocument.save();
                return truck;
            }

        } catch (error) {
            throw error;
        }

    }

    public async delete(truck: Truck): Promise<Truck> {
        const query = { domainId: truck.id.toString()};
        const truckDocument = await this.truckSchema.findOne( query );
        try {
            if(truckDocument === null) {
                return truck;
            }
            else{
                await truckDocument.remove();
                return truck;
            }

        } catch (error) {
            throw error;
        }
    }

    public async getTruckById(id: string): Promise<Truck> {
        const query = { domainId: id};
        const truckDocument = await this.truckSchema.findOne( query as FilterQuery<ITruckPersistence & Document>);

        if(truckDocument === null) {
            return null;
        }
        else{
            return TruckMap.toDomain(truckDocument);
        }
    }



}