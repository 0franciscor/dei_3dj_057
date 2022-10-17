using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using EletricGo.Domain.Shared;
using EletricGo.Domain.Warehouse;

namespace EletricGo.Domain.Warehouse
{
    public class Warehouse : Entity<WarehouseID>, IAggregate {
        public WarehouseID warehouseID { get; private set; }
        
        public WarehouseDesignation warehouseDesignation { get; private set; }
        
        public Warehouse(WarehouseID warehouseID, WarehouseDesignation warehouseDesignation)
        {
            this.warehouseID = warehouseID;
            this.warehouseDesignation = warehouseDesignation;
        }
        
        public Warehouse(WarehouseDTO warehouseDTO)
        {
            this.warehouseID = new WarehouseID(warehouseDTO.warehouseID);
            this.warehouseDesignation = new WarehouseDesignation(warehouseDTO.warehouseDesignation);
        }
        
        public WarehouseDTO toWarehouseDTO()
        {
            return new WarehouseDTO(this.warehouseID.AsGuid(), this.warehouseDesignation.Value);
        }
    }
}