import { IPackagingPersistence } from "../../dataschema/IPackagingPersistence";
import mongoose from "mongoose";

const PackagingSchema = new mongoose.Schema (
    {
        
        domainId: {
            type: String,
            unique: true
        },
        packagingID: {
            type: String,
            unique: true,
            required: true
        },
        truckID: {
            type: String,
            unique: true,
            required: true
        },
        deliveryID: {
            type: String,
            unique: true,
            required: true
        },
        xPosition: {
            type: Number,
            required: true
        },
        yPosition: {
            type: Number,
            required: true
        },
        zPosition: {
            type: Number,
            required: true
        }
    },
    {
        timestamps: true
    }
);


export default mongoose.model<IPackagingPersistence & mongoose.Document>('Packaging', PackagingSchema);