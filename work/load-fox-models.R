dag_foxes = dagitty('
dag {
    bb="0,0,1,1"
    area [pos="0.500,0.200"]
    avgfood [pos="0.300,0.250"]
    groupsize [pos="0.700,0.250"]
    weight [outcome,pos="0.500,0.300"]
    area -> avgfood
    avgfood -> groupsize
    avgfood -> weight
    groupsize -> weight
}')

data(foxes)
fox_df <- foxes
fox_df$Area <- standardize( fox_df$area )
fox_df$Avgfood <- standardize( fox_df$avgfood )
fox_df$Groupsize <- standardize( fox_df$groupsize )
fox_df$Weight <- standardize( fox_df$weight )

mfox_Weight_Area <- quap(
    alist(
        Weight ~ dnorm(mu, sigma),
        mu <- b0 + bArea*Area,
        b0 ~ dnorm(0, 0.1),
        bArea ~ dnorm(0, 0.5),
        sigma ~ dunif(0, 5)
    ) , data=fox_df )
mfox_Weight_Avgfood <- quap(
    alist(
        Weight ~ dnorm( mu , sigma ) ,
        mu <- bAvgfood*Avgfood,
        bAvgfood ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dunif( 0 , 5 )
    ) , data=fox_df )
mfox_Weight_AvgfoodGroupsize <- quap(
    alist(
        Weight ~ dnorm( mu , sigma ) ,
        mu <- bAvgfood*Avgfood + bGroupsize*Groupsize,
        bAvgfood ~ dnorm( 0 , 0.5 ) ,
        bGroupsize ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dunif( 0 , 5 )
    ) , data=fox_df )
mfox_Weight_GroupsizeArea <- quap(
    alist(
        Weight ~ dnorm( mu , sigma ) ,
        mu <- bGroupsize*Groupsize + bArea*Area,
        bGroupsize ~ dnorm( 0 , 0.5 ) ,
        bArea ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dunif( 0 , 5 )
    ) , data=fox_df )
mfox_Weight_AvgfoodGroupsizeArea <- quap(
    alist(
        Weight ~ dnorm( mu , sigma ) ,
        mu <- bAvgfood*Avgfood + bGroupsize*Groupsize + bArea*Area,
        bAvgfood ~ dnorm( 0 , 0.5 ) ,
        bGroupsize ~ dnorm( 0 , 0.5 ) ,
        bArea ~ dnorm( 0 , 0.5 ) ,
        sigma ~ dunif( 0 , 5 )
    ) , data=fox_df )
