import { ITruckPersistence } from "../../dataschema/ITruckPersistence";
import mongoose from "mongoose";

const TruckSchema = new mongoose.Schema (
    {
        
        domainId: {
            type: String,
            unique: true
        },
        truckID: {
            type: String,
            unique: true,
            required: true
        },
        tare: {
            type: Number,
            required: true
        },
        capacity: {
            type: Number,
            required: true
        },
        maxBatteryCapacity: {
            type: Number,
            required: true
        },
        autonomy: {
            type: Number,
            required: true
        },
        fastChargeTime: {
            type: Number,
            required: true
        }

    },
    {
        timestamps: true
    }
);


export default mongoose.model<ITruckPersistence & mongoose.Document>('Truck', TruckSchema);