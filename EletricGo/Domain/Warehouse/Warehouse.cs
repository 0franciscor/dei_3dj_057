using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using EletricGo.Domain.Shared;
using EletricGo.Domain.Warehouse;

namespace EletricGo.Domain.Warehouse
{
    public class Warehouse : Entity<WarehouseID> {
        
        public WarehouseID warehouseID { get; private set; }

        public WarehouseAddress warehouseAddress { get; private set;}
        
        public WarehouseDesignation warehouseDesignation { get; private set; }
        
        public Warehouse(WarehouseID warehouseID,WarehouseAddress warehouseAddress, WarehouseDesignation warehouseDesignation)
        {
            this.warehouseID = warehouseID;
            this.warehouseAddress = warehouseAddress;
            this.warehouseDesignation = warehouseDesignation;
        }
        
        public Warehouse(WarehouseDTO warehouseDTO)
        {
            this.warehouseID = new WarehouseID(warehouseDTO.warehouseID);
            this.warehouseAddress = new WarehouseAddress(warehouseDTO.address);
            this.warehouseDesignation = new WarehouseDesignation(warehouseDTO.designation);
        }
        
        public WarehouseDTO toWarehouseDTO()
        {
            return new WarehouseDTO(this.warehouseID.AsGuid(), this.warehouseAddress.address,this.warehouseDesignation.designation);
        }
    }
}