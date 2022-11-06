import { Document } from "mongodb";
import { Model } from "mongoose";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { Mapper } from "../core/infra/Mapper";
import { IRoutePersistence } from "../dataschema/IRoutePersistence";
import { Route } from "../domain/route/Route";
import { IRouteDTO } from "../dto/IRouteDTO";



export class RouteMap extends Mapper<Route> {
    
    public static toDTO(route: Route): IRouteDTO {
        return {
            id: route.id.toString(),
            routeID: route.routeID.id,
            date: route.date.date,
            warehouses: route.warehouse.warehouse,

        } as IRouteDTO;
    }
    
    public static toDTOList(routes: Route[]):IRouteDTO[]{
        return routes.map((route) => RouteMap.toDTO(route));
    }

    public static toDomain(route: any | Model<IRoutePersistence & Document>): Route {
        const routeOrError = route.create(
            route,
            new UniqueEntityID(route._id),
        );
        routeOrError.isFailure ? console.log(routeOrError.error) : '';

        return routeOrError.isSuccess ? routeOrError.getValue() : null;
    }

    public static toPersistence(route: Route): any{
        return {
            domainId: route.id.toString(),
            routeID: route.routeID.id,
            date: route.date.date,
            warehouses: route.warehouse.warehouse,
            
        };
    }
        
}
    


