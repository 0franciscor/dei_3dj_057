import mongoose from "mongoose";
import { IPathPersistance } from "../../dataschema/IPathPersistance";


const PathSchema = new mongoose.Schema (
    {
        domainId:{
            type:String,
            unique: true
        },
        pathID:{
            type: String,
            unique: true,
            required: true
        },
        startWHId: {
            type: String,
            unique:true,
            required: true
        },
        destinationWHId: {
            type: String,
            unique:true,
            required: true
        },
        pathDistance:{
            type: Number,
            required: true
        },
        pathTravelTime:{
            type: Number,
            required: true
        },
        wastedEnergy:{
            type:Number,
            required: true
        },
        extraTravelTime:{
            type: Number,
            required: true
        }
    },
    {
        timestamps: true
    }
);

export default mongoose.model<IPathPersistance & mongoose.Document>('Path',PathSchema);