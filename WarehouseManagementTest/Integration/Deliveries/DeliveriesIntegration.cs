using EletricGo.Controllers;
using EletricGo.Domain.Deliveries;
using Moq;
using EletricGo.Domain.Shared;
using Microsoft.AspNetCore.Mvc;

namespace WarehouseManagementTest.Controllers.Deliveries
{
    [TestFixture]
    internal class DeliveriesIntegration
    {
        private string? id;

        private DateTime deliveryDate;

        private float loadTime;

        private float unloadTime;

        private string? destination;

        private float deliveryMass;

        [SetUp]
        public void Setup()
        {
            id = "testID";
            deliveryDate = new DateTime(2020, 12, 12);
            loadTime = 10;
            unloadTime = 20;
            destination = "testDestination";
            deliveryMass = 30;

        }

        //SETUP LISTS FOR GETALL()
        private List<Delivery> GetDeliveriesList()
        {
            var deliveryList = new List<Delivery>
            {
                new Delivery(id, new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass)),
                new Delivery("id2", new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass)),
                new Delivery("id3", new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass)),
                new Delivery("id4", new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass)),
                new Delivery("id5", new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass)),
            };

            return deliveryList;
        }

        [Test]
        public void DefineDriverServiceConstrutor()
        {
            var mockRepository = new Mock<IDeliveryRepository>();
            var mockUnit = new Mock<IUnitOfWork>();

            var service = new DeliveryService(mockUnit.Object, mockRepository.Object);

            Assert.That(service, Is.Not.Null);
        }

        [Test]
        public async Task GetAllTest()
        {
            var expectedList = GetDeliveriesList();

            var mockUnit = new Mock<IUnitOfWork>();
            var mockRepository = new Mock<IDeliveryRepository>();
            mockRepository.Setup(repo => repo.GetAll()).ReturnsAsync(expectedList);

            var deliveryService = new DeliveryService(mockUnit.Object, mockRepository.Object);
            var deliveryController = new DeliveryController(deliveryService);

            var resultList = await deliveryController.GetAll();

            for(int i = 0; i < expectedList.Count; i++)
            {
                Assert.AreEqual(expectedList[i].Id, resultList.Value[i].deliveryID);
            }
        }
        
    }
}
