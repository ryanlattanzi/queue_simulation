using PyPlot
###############################################################################

println("We would like our avg waiting time to be within plus/minus 0.083 mins (plus/minus 5 seconds)")
l = 0.166
println("Hence, our confidence interval will have length: ", l)

########## TWO SERVERS IN SERIES (CURRENT MODEL) ###########

using Compat, Distributions

function sim1()
    t = 0; n1 = 0; n2 = 0; Na = 0; Nd = 0;
    tA = t - ((6/7)*log(rand()));
    tF = Inf; tD = Inf;
    Arriv = [];
    Dep = [];

    while true

        # CASE 1
        if (tA == minimum([tA,tF,tD]))
            t = tA
            Na = Na + 1
            n1 = n1 + 1
            tA = t - ((6/7)*log(rand()))

            if tA > 120
                tA = Inf
            end

            if n1 == 1
                tF = t + abs(rand(Normal(0.75,0.25)))
            end

            push!(Arriv, t)

        # CASE 2
        elseif (tF == minimum([tA,tF,tD]))
            t = tF
            n1 = n1-1
            n2 = n2+1

            if (n1 == 0)
                tF = Inf
            else
                u = rand()
                tF = t + abs(rand(Normal(0.75,0.25)))
            end

            if (n2 == 1)
                u = rand()
                tD = t + abs(rand(Normal(0.75,0.25)))
            end

        # CASE 3
        elseif (tD == minimum([tA,tF,tD]))
            t = tD
            Nd = Nd + 1
            n2 = n2 - 1

            if n2 == 0
                tD = Inf
            elseif n2 > 0
                u = rand()
                tD = t + abs(rand(Normal(0.75,0.25)))
            end

            push!(Dep, t)

            if (tA > 120) && (n2 == 0) && (n1 == 0)
                break
            end
        end
    end

    return Arriv, Dep

end

N = 100
Avgwait = zeros(N)
cust = zeros(N)
timeafter = zeros(N)

for i in 1:N

    Arriv, Dep = sim1()
    Wait = Dep-Arriv
    Avg = mean(Wait)
    Avgwait[i] = Avg
    taft = Dep[end] - 120
    timeafter[i] = taft
    k = length(Arriv)
    cust[i] = k
end

println("SIMULATION 1: ")

# So, we have generated 100 values to compute a std dev of the avg waiting times
S = std(Avgwait)

# Length of the interval with current std dev
len = (2*1.96*S)/sqrt(N)


while (len >= l)

    Arriv, Dep = sim1()

    Wait = Dep-Arriv
    Avg = mean(Wait)
    push!(Avgwait, Avg)
    taft = Dep[end] - 120
    push!(timeafter, taft)
    k = length(Arriv)
    push!(cust, k)

    S = std(Avgwait)
    N = N + 1
    len = (2*1.96*S)/sqrt(N)
end

N # Gives number of runs it took

println("This simulation ran ",N," times.")

stddev = std(Avgwait)
m = mean(Avgwait)

lbound = m-(1.96*stddev)/sqrt(N)
ubound = m+(1.96*stddev)/sqrt(N)

ConfInt = [lbound,ubound]
println("Expected waiting time: ", mean(m))
println("95 % confidence interval for expected waiting time: ",ConfInt)

mcust = mean(cust)
println("Average number of customers after ", N ," runs: ", mcust)

println("Average number of minutes after the two hour period that the last customer leaves: ",mean(timeafter))

###############################################################################

########## TWO PARALLEL SERVERS ###########

function sim2()
    t = 0;Na = 0;C1 = 0;C2 = 0;
    i1 = 0;i2 = 0;SS = [0,0,0]

    Arrivals = zeros(500);
    Departures = zeros(500);
    tA = t - ((6/7)*log(rand()));
    t1 = Inf;
    t2 = Inf;

    while true

        if (tA == minimum([tA,t1,t2]))

            t = tA
            Na = Na + 1
            Arrivals[Na] = t
            u = rand()
            tA = t - ((6/7)*log(u))

            if tA > 120
                tA = Inf
            end

            if (SS == [0,0,0])
                SS[1] = 1
                i1 = Na
                SS[2] = i1
                t1 = t + abs(rand(Normal(1.5,0.5)))

            elseif (SS[1] == 1) & (SS[2] != 0) & (SS[3] == 0)
                SS[1] = 2
                i2 = Na
                SS[3] = i2
                t2 = t + abs(rand(Normal(1.5,0.5)))

            elseif (SS[1] == 1) & (SS[2] == 0) & (SS[3] != 0)
                SS[1] = 2
                i1 = Na
                SS[2] = i1
                t1 = t + abs(rand(Normal(1.5,0.5)))
            elseif (SS[1] > 1)
                SS[1] = SS[1] + 1
            end

        elseif (t1 == minimum([tA,t1,t2]))

            t = t1
            C1 = C1 + 1
            Departures[i1] = t

            if (SS[1] == 1)
                SS = [0,0,0]
                t1 = Inf
            elseif (SS[1] == 2)
                SS[1] = 1
                SS[2] = 0
                t1 = Inf
            elseif (SS[1] > 2)
                m = maximum([i1,i2])
                SS[1] = SS[1] - 1
                i1 = m + 1
                SS[2] = i1
                t1 = t + abs(rand(Normal(1.5,0.5)))
            end

        elseif (t2 == minimum([tA,t1,t2]))

            t = t2
            C2 = C2 + 1
            Departures[i2] = t

            if (SS[1] == 1)
                SS = [0,0,0]
                t2 = Inf
            elseif (SS[1] == 2)
                SS[1] = 1
                SS[3] = 0
                t2 = Inf
            elseif (SS[1] > 2)
                m = maximum([i1,i2])
                SS[1] = SS[1] - 1
                i2 = m + 1
                SS[3] = i2
                t2 = t + abs(rand(Normal(1.5,0.5)))
            end
        end
        if (SS == [0,0,0]) & (tA == Inf)
            break
        end
    end
    filter!(e->e≠0.0,Arrivals);
    filter!(e->e≠0.0,Departures);

    return C1, C2, Arrivals, Departures
end

N2 = 100
Averagewait = zeros(N2)
cus = zeros(N2)
tafter = zeros(N2)
count1 = zeros(N2)
count2 = zeros(N2)

for i in 1:N2

    C1, C2, Arrivals, Departures = sim2()
    Waiting = Departures-Arrivals
    Avgg = mean(Waiting)
    Averagewait[i] = Avgg
    taftt = Departures[end] - 120
    tafter[i] = taftt
    k = length(Arrivals)
    cus[i] = k
    count1[i] = C1
    count2[i] = C2

end

println("SIMULATION 2: ")
# So, we have generated 100 values to compute a std dev of the avg waiting times
S = std(Averagewait)
# Length of the interval with current std dev
len = (2*1.96*S)/sqrt(N2)

while (len >= l)

    C1, C2, Arrivals, Departures = sim2()

    Waiting = Departures-Arrivals
    Avgg = mean(Waiting)
    push!(Averagewait, Avgg)
    taftt = Departures[end] - 120
    push!(tafter, taftt)
    k = length(Arrivals)
    push!(cus, k)
    push!(count1, C1)
    push!(count2, C2)

    S = std(Averagewait)
    N2 = N2 + 1
    len = (2*1.96*S)/sqrt(N2)
end



N2 # Gives number of runs it took
println("This simulation ran ",N2," times.")

stddev = std(Averagewait)
m = mean(Averagewait)

lbound = m-(1.96*stddev)/sqrt(N2)
ubound = m+(1.96*stddev)/sqrt(N2)

ConfInt = [lbound,ubound]
println("Expected waiting time: ", mean(m))
println("95 % confidence interval for expected waiting time: ",ConfInt)

mcus = mean(cus)
println("Average number of customers after ", N2 ," runs: ", mcus)

println("Average number of minutes after the two hour period that the last customer leaves: ",mean(tafter))

println("Server 1 served on average ", mean(count1), "customers.")
println("Server 2 served on average ", mean(count2), "customers.")


r = collect(1:N)

p = plt[:hist](Avgwait,30)
title("Two Servers in Series")
xlabel("Avg Wait Time")
plt[:show](p)

z = collect(1:N2)

q = plt[:hist](Averagewait,30)
title("Two Parallel Servies")
xlabel("Avg Wait Time")
plt[:show](q)
