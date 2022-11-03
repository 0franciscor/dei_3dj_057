import { AggregateRoot } from "../../core/domain/AggregateRoot";
import { UniqueEntityID } from "../../core/domain/UniqueEntityID";
import { Result } from "../../core/logic/Result";
import { IPackagingDTO } from "../../dto/IPackagingDTO";
import { PackagingID } from "./PackagingID";
import { TruckID } from "../truck/TruckID";
import { DeliveryID } from "./DeliveryID";
import { XPosition } from "./XPosition";
import { YPosition } from "./YPosition";
import { ZPosition } from "./ZPosition";


interface PackagingProps {
  packagingID: PackagingID;
  truckID: TruckID;
  deliveryID: DeliveryID;
  xPosition: XPosition;
  yPosition: YPosition;
  zPosition: ZPosition;
}

export class Packaging extends AggregateRoot<PackagingProps> {
  get id (): UniqueEntityID {
    return this._id;
  }

  get packagingID (): PackagingID {
    return this.props.packagingID;
  }

  get truckID(): TruckID {
    return this.props.truckID;
  }

  get deliveryID(): DeliveryID {
    return this.props.deliveryID;
  }

  get xPosition(): XPosition {
    return this.props.xPosition;
  }
  get yPosition(): YPosition {
    return this.props.yPosition;
  }
  get zPosition(): ZPosition {
    return this.props.zPosition;
  }


  set truckID(truckID: TruckID) {
    this.props.truckID = truckID;
  }

  set deliveryID(deliveryID: DeliveryID) {
    this.props.deliveryID = deliveryID;
  }

  set xPosition(xPosition: XPosition) {
    this.props.xPosition = xPosition;
  }
  
  set yPosition(yPosition: YPosition) {
    this.props.yPosition = yPosition;
  }

  set zPosition(zPosition: ZPosition) {
    this.props.zPosition = zPosition;
  }
  


  private constructor (props: PackagingProps, id?: UniqueEntityID) {
    super(props, id);
  }

  public static create (PackagingDTO:IPackagingDTO , id?: UniqueEntityID): Result<Packaging> {

      

      try {
        const packaging = new Packaging({
          packagingID: PackagingID.create(PackagingDTO.packagingID).getValue(),
          truckID: TruckID.create(PackagingDTO.truckID).getValue(),
          deliveryID: DeliveryID.create(PackagingDTO.deliveryID).getValue(),
          xPosition: XPosition.create(PackagingDTO.xPosition).getValue(),
          yPosition: YPosition.create(PackagingDTO.yPosition).getValue(),
          zPosition: ZPosition.create(PackagingDTO.zPosition).getValue(),
        }, id);
    
        return Result.ok<Packaging>(packaging);
      } catch (error) {
        return Result.fail<Packaging>(error);
      }
        
    
     


  }



}