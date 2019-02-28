;MIT License

;Copyright (c) 2019 Jasper Meijering

;Permission is hereby granted, free of charge, to any person obtaining a copy
;of this software and associated documentation files (the "Software"), to deal
;in the Software without restriction, including without limitation the rights
;to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;copies of the Software, and to permit persons to whom the Software is
;furnished to do so, subject to the following conditions:

;The above copyright notice and this permission notice shall be included in all
;copies or substantial portions of the Software.

;THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;SOFTWARE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Used packages, agents and variables;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

extensions [ gis ]                               ; load gis extention
undirected-link-breed [active-links active-link] ; use links to build social networks amongst programme managers

breed [PMs PM]
breed [IMs IM]
breed [RSs Rs]
breed [DoIAs DoIA]
breed [city-labels city-label ]
breed [country-labels country-label ]


PMs-own [     age                               ; number of ticks the agent is deployed
              die-at-age                        ; will the pm leave this tick, normal distribution with variable mean and sd.
              organisation                      ; name of organisation where the agent belongs to
              colleagues                        ; agentset of pms that are a colleague
              network                           ; agentset of pms that are in a pms network
              need-here                         ; ground truth need on this patch
              assessed-need-here                ; need on this patch accroding to the pm, dependend on pms capacity
              capacity-pm                       ; the accurracy of the pm's assessment, mean normal distribution
              last-assessment                   ; list of information (id, location, tick, need, activity)
              memory                            ; list of information-items: list of lists the latter of the form (id, location, tick, need, activity)
              recently-self-assessed            ; list of information-items that have been produced by this pm.
              recently-self-assessed_for_pms    ; list of information-items that have been produced by this pm and will be shared with other pms (needed for procedure).
              gap_list                          ; list of all gaps that are in my memory
              max_gap_value                     ; max gap of all gaps that are in my memory
              next_location                     ; the next location that I will visit for to start an activity
              requester_of_remote_support       ; bolean, if yes: I'm the requester of remote support.
              successor
              my-activities
              ]

IMS-own [     organisation                      ; name of the organisation IM belongs to
              colleagues                        ; agentset of pms that work for the same organisation as the IM
        ]

RSs-own [     ;requester
              ;capacity-rs
]

DoIAs-own [   organisation                      ; the organistion that the DoIA belongs to.

]


patches-own [ needs                             ; ground truth need on the patch measured in number of people
              activities                        ; ground truth activity on the patch measured in number of people
              gap-patches                       ; gap at this patch according to the ground truths
              redundancies_patch                ; at patch, activities minus needs if activities > needs.
              is-epicenters?                    ; bolean is this patch currentlty an epicenter of a shock
              block_id                          ; gis: id of the block. Randomly taken from the shapefile
              activity-started-this-tick]       ; is there a activity being started here this tick?



globals [


              ;new-PM-connections-per-tick      ; numberof fellow PMs a PM will get to know each tick -> to number of ticks
              ;leave-chance                     ; constant chance of leaving
              ;min_number_of_PMs                ; number of living PMs in the system
              ;total-people-in-need             ; total number in need.
              ;epicenters                       ; number of epicenters per shock
              ;disaster-length                  ; number of ticks of the simulation
              ;shocks                           ; numnber of shocks in the disaster
              ;random-disaster?                 ; bolean will the the disaster generated for this number of shocks and number of epicenters be pseuo-random or not?
              ;random-location-shocks?          ; bolean will the location of the shocks in this disaster be pseuo-random or not?
              ;show-PMs?                        ; bolean do you want to see the PMs
              ;show-connections?                ; bolean do you want ot see the connections betwen the PMs
              ;sd-accuracy-pms                  ; the sd of the capacity of the pms. How much does capacity differ amongst PMs.
              ;forget-after-n-ticks             ; Treshold for forgeting information. All information that is older then this treshold is forgotten by the pms
              ;extend-network-every-n-ticks     ; number of ticks between a event where all pms make a new connections.
              ;length_IM_reporting_cycle        ; number of ticks between to IM information requests/publications
              ;stay-connected?                  ; if true -> if a pm has no connection beceuase his partner left, let that pm look for new partner
              ;extend-network?                  ; if true -> pms extend their networks every n ticks
              ;average_number_of_remote_assessments

              ;length_activity                  ; number of ticks between two activities
              ;size_activity                    ; percentage of the needs that will be covered by the activity (100% all the needs at that patch (at that tick) will be covered)
              ;disaster-scenario                ; name of the disaster scenario
              ;mean_deployment_length           ; mean length of deployment. Mean for normal distribution
              ;sd_deployment_length             ; sd of deployment. SD for normal distrbution
              ;need-diffuse-coefficient         ; how much of the needs will be diffused to neighboring pathces, at each tick
              ;show-map                         ; bolean, yes map is displayed
              ;show-names                       ; bolean yes names of camps is displayed
              ;deployment-frequency             ; who often are new pms send to the field (if 3 then every 3rd tick new pms are send).
              ;share-international-local        ; percentage of the pms that is local vs international. Local pms have longer mean deployement lengths.
              ;assessment_length
              ;Willingness_focus

              max-need-patches                  ; highest need of all patches
              min-need-patches                  ; lowest need of all patches
              max-gap-patches                   ; highest gap of all patches
              min-gap-patches                   ; lowest gap of all patches
              disaster                          ; architecture of the disaster. At which tick there is a shock?
              max_gap_location                  ; location of the patch with the biggest gap
              needs_locations_list              ; list with needs
              number-shock                      ; counter of loop
              ticks-since-shock                 ; how many tick since a shock will struck? In the beginning: number of ticks since the start of the simulation
              ticks-to-shock                    ; how many tick from now a shock will strike? At the end: number of ticks to end of the simulation -1
              counter                           ; counter of loop
              number-undeployed                 ; counter of dead PMs
              organisations                     ; list of names of humanitarian organisation
              id                                ; id for information
              i                                 ; counter
              j                                 ; counter
              information_exchange_W3           ; auxilary variable this information will be shared in the w3
              stop_condition                    ; auxilary variable to set stop and update stop condiation of procedures
              stop_condition2                   ; auxilary variable to set stop and update stop condiation of procedures
              new-partner                       ; auxilary variable that remembers a pms new partner
              W3                                ; list with needs + activities
              W3_clean                          ; cleaned list with needs + activities
              gap_locations                     ; auxilary variable list of location (pathces) where I know there are gaps.
              total_needs                       ; sum of needs at all patches
              total_activities                  ; sum of activities at all patches
              total_gap                         ; sum of gaps at all patches including negative gaps.
              total_redundancies                ; sum of all redundancies at all patches
              information_diffused_by_ims       ; number of information items shared by ims
              information_diffused_by_pms       ; number of information items shared by pms
              information_diffused_by_rs        ; number of information items shared by rs
              total_information_diffused        ; sum of information items shared by ims pms and rs
              remote-assessment                 ; assessment of needs at all patches, conducted by rs
              capacity-rs                       ; accuracy of assessments of rs, normal distribution
              sd-capacity-rs                    ; standard deviation of the accuracy of remote support.
              request_remote_support            ; auxilary vairable, bolean if true remote support has been requested.
              publish_remote_support            ; tick at which remote assessment will be published.
              people-in-need                    ; total number in need
              total_gap_over_time               ; sum of gap at each tick, intergral of total gaps.
              camps-dataset                     ; shapefle of camps in Bangladesh
              blocks-dataset                    ; shapefile of camp's blocks in Bangladesh
              list_block_id                     ; list of all block ids from blocks-dataset
              max_gap_position_list             ; auxilary variable position of max gap value in memory
              information_exchange              ; auxilary variable this information will be shared among pms
              days_worked                       ; total days worked by pms, sum of pms each day.
              mean_deployment_length_local      ; mean deployment length of local pms
              mean_deployment_length_inter      ; mean deployment length of international pms
              organisation-list                 ; list with all the locations acitive in the response


]

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;
to setup
  clear-all
  reset-ticks
  ; Note that it is not required to load new GIS maps / layers into the model to change the parametrisation to reflect a different disaster.
  ; The GIS extension mainly serves visualisation purpuses.
  ; Note that setting the coordinate system here is optional, as
  ; long as all of your datasets use the same coordinate system.
  gis:load-coordinate-system (word "data/Sub_Mahjee_Block_Boundary.prj")
  ; Load all of our datasets
  set camps-dataset gis:load-dataset "data/181001_Outline_Rohingya_Refugee_Camp_A1.shp"
  ;set blocks-dataset gis:load-dataset "data/Sub_Mahjee_Block_Boundary.shp" ; sub blocks instead of blocks.
  set blocks-dataset gis:load-dataset "data/Mahjee_Block_Boundary.shp"
  gis:set-world-envelope (gis:envelope-union-of (gis:envelope-of blocks-dataset)
                                                (gis:envelope-of camps-dataset))
  if show-map = True [
  display-camps
  display-blocks
  ]

  ; load the scenerio and disaster
  ifelse random-disaster? [] [random-seed 1000]
  load-scenario
  load-disaster

  ; setup globals
  set-default-shape PMs "person service"
  set-default-shape IMs "house two story"
  set-default-shape RSs "computer workstation"
  set-default-shape DoIAs "person business"

  set organisation-list ["AFC" "BRAC" "DSK" "IOM" "NGOF" "OXFAM" "SCI" "SHED" "SI" "other"]
  set organisations n-of number-of-organisations organisation-list ;
  set ticks-since-shock 0
  ;set mean_deployment_length 90
  ;set sd_deployment_length 30
  set counter 1
  set ticks-to-shock 1
  ;set need-diffuse-coefficient 0.8
  set number-undeployed 0
  set request_remote_support false
  set publish_remote_support -1
  set people-in-need ((total-people-in-need / shocks) / epicenters)
  set days_worked 0
  set mean_deployment_length_local 220
  set mean_deployment_length_inter  90
  set remote-assessment [ ]
  set information_diffused_by_rs 0
  ; create first PMS
  make-node nobody                            ; first  PM, unattached
  make-node PM 0                              ; second PM, attached to first node

  ; create other PMs
  repeat (min_number_of_PMs - 2) [
  make-node find-partner
  ]

  ; create IMs
  create-IMs length organisations
  create-RSs 1
  create-DoIAs length organisations
  while [item counter disaster != 1] [
    set ticks-to-shock (ticks-to-shock + 1)
    set counter (counter + 1)
        ]

  ask PMs [ setup-pms ]
  ask one-of PMs [set requester_of_remote_support True]

  ; set publicaiton method
  if RS_publication_method = "Time-focused" [
        ; fast, not accurate
    set assessment_length 5 ; fast
    set sd-accuracy-pms 8 ; not accurate
    set sd-capacity-rs 8 ; not accurate
        show "test"
      ]

   if RS_publication_method = "Accuracy-focused" [
         ; Accuracy, not accurate
    set assessment_length 10 ; slow
    set sd-accuracy-pms 2 ; accurate
    set sd-capacity-rs 2 ; accurate
      ]

   if Willingness_focus = "Inward" [
    set willingness_to_share_intra-org 70
    set willingness_to_share_inter-org 45
  ]

  if Willingness_focus = "Outward" [
    set willingness_to_share_inter-org 70
    set willingness_to_share_intra-org 45
  ]

  setup-ims
  setup-rss
  setup-DoIAs
  ask patches [setup-patches]
  update-globals
  update-pms
  update-visuals
  reset-ticks
end

to load-scenario
   ifelse random-disaster? [] [random-seed 1000]
  if scenario = True [
  if disaster-scenario = "Bangladesh-like" [
      print "Loading Bangladesh-like scenario:"
  set disaster-length 2 * 365
  set shocks 12
  set epicenters 2
  set total-people-in-need 1000000
  set mean_deployment_length 90
  set sd_deployment_length 6
  set extend-network-every-n-ticks 14
  set stay-connected? True
  set extend-network? True
  set connect-to-colleagues? true
  set length_IM_reporting_cycle 14
  set size_activity 100
  set length_activity 4
  set sd-capacity-rs 4
  set average_number_of_remote_assessments 4
  set PMs-per-tick-per-DoIA 1
  set goal 0.15
  set need-diffuse-coefficient 0.2

  ; set policies
  set willingness_to_share_inter-org 45
  set willingness_to_share_intra-org 45

  ; add rss diffused to total diffused (strong effect?) multiple lines in one plot
  ; To do: What if remote_assessments are close to eachoter?

      set disaster n-values (disaster-length - shocks) [0]
      set number-shock shocks
      while [number-shock > 0] [
        set disaster insert-item random(length disaster) disaster 1
        set number-shock (number-shock - 1 )
      ]
  ]
  if disaster-scenario = "Bangladesh" [
      print "Loading Bangladesh scenario: "
    set disaster [0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 1 0 0
      0 1 0 0 1 0 0 1 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]

  ]
    print disaster
  ]

end

to load-disaster
  if scenario = False [
  ; create a list that represents the disaster.
  ;'A '1' corresponts to a tick with shock, a '0' corresponts to a tick without a shock.
  set disaster n-values (disaster-length - shocks) [0]
  set number-shock shocks
   while [number-shock > 0] [
     set disaster insert-item random(length disaster) disaster 1
     set number-shock (number-shock - 1 )
  ]
    print "Loading the following scenario:"
    print disaster
  ]

end

to setup-pms
    ;   give the new pm an organisation, corresponding color, capacity, memory ..
    ifelse ticks < 1
    [set die-at-age round random mean_deployment_length] ; at tick 0 die-at-age random prevent that deployments are synct. Beware that this could at some noise, as this is a extra random function.
    [set die-at-age round random-normal (mean_deployment_length) sd_deployment_length] ;
    set organisation one-of organisations
    set color scale-color yellow (position (organisation) organisations) 0 length organisations
    if distribution-accuracy = 1 [
     set capacity-pm ((random-normal 100 sd-accuracy-pms) / 100 )]
    if distribution-accuracy = 2 [
     set capacity-pm ((random-normal 90 sd-accuracy-pms) / 100 )]
    if distribution-accuracy = 3 [
     set capacity-pm ifelse-value (random-float 1 < 0.5)
      [((random-normal 80 sd-accuracy-pms) / 100)]
      [((random-normal 120 sd-accuracy-pms) / 100)]
     ]
  if distribution-accuracy = 4 [
     set capacity-pm ifelse-value (random-float 1 < 0.75)
      [((random-normal 80 sd-accuracy-pms) / 100)]
      [((random-normal 120 sd-accuracy-pms) / 100)]
     ]
  if distribution-accuracy = 5[
     set capacity-pm ifelse-value (random-float 1 < 0.25)
      [((random-normal 80 sd-accuracy-pms) / 100)]
      [((random-normal 120 sd-accuracy-pms) / 100)]
     ]

    set memory []
    set recently-self-assessed []
    set recently-self-assessed_for_pms []
    set information_exchange [ ]
    set gap_list [ ]
    set requester_of_remote_support false
  set my-activities [ ]

end

to setup-ims
  ; give the new im its attributes
  let list_ims sort-on [who] IMs
  set i 0
  set stop_condition length list_ims - 1
  while [i <= stop_condition] [
    ask item i list_ims  [set organisation item i organisations]
  set i i + 1]
  ask IMs [
    set xcor max-pxcor - (position organisation organisations )
    set ycor max-pycor
    set size size * 1
    set W3 [ ]
  ]
end

to setup-rss
  ; give the new rss its attributes
  ask rss [
  set xcor max-pxcor
  set ycor min-pycor
  set size size * 1
  ]
end

to setup-DoIAs
  ; give the new DoIA its attributes
  let list_doias sort-on [who] DoIAs
  set i 0
  set stop_condition length list_doias - 1
   while [i <= stop_condition] [
    ask item i list_doias  [set organisation item i organisations]
  set i i + 1]

  ask doias [
  set xcor min-pxcor + (position organisation organisations )
  set ycor min-pycor
  set size size * 1
  ]

end

to setup-patches
  ;    give all patches a ground truth needs and make them default value for is epicenter of false.
   set needs 0
   set is-epicenters? false
end


;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Procedure  ;;;
;;;;;;;;;;;;;;;;;;;;;;; ; TO DO make order consitent.

to go
  if ticks = length disaster [stop]
  clear-output

  shock
  perform-assessment
  exchange-information-pm-pm
  exchange-information-pm-im
  publish-w3
  forget-information
  hand-over-policy
  leave-disaster
  deploy-pm
  make-connection
  connect-to-colleagues
  request-remote-support
  perform-remote-assessment
  publish-remote-assessment
  run-operations
  diffuse-needs
  update-reporters
  update-globals
  update-pms
  update-ims
  update-visuals
  tick
end

to deploy-pm
  ; the DoIA sends PMs to the field. The PMs link to one infield pm.
  ; There is a minimum number of pms in the field: min_number_of_PMs

  ask DoIAs [
    if remainder ticks deployment-frequency = 0 [
   ifelse total_gap > goal * total_needs [
  make-pm find-new-partner
    ][
      if count pms < min_number_of_PMs [
        make-pm find-new-partner
      ]
    ]
  ]
 ]
end

to make-node [old-node]
  ; only make-node first time.
  ; go to a random location
  create-PMs 1
  [
    if old-node != nobody
      [ create-active-link-with old-node [ set color green ]
      ]
    setup-pms
    setxy random-xcor random-ycor
  ]

end

to make-pm [old-node]
  ; create a new PM.
  ; PMs are deployed at the location of their organisations IM
  hatch-PMs PMs-per-tick-per-DoIA
  [
    if old-node != nobody
      [ create-active-link-with old-node [ set color green ]
        setup-pms
        let start_turtle one-of IMs with [organisation = [organisation] of myself]
        move-to start_turtle ; PMs are deployed at the location of their organisations IM
  ]
  ]


end

to perform-assessment
  ; Move to a location where I don't know the needs.
  ; Get needs from patch here.
  ; Multiply with capacity.
  ; Store in memmory with tick and location
  if remainder ticks assessment_length = 0 [
  ask PMs [
      set i 0
      set stop_condition length memory - 1
      set needs_locations_list [self] of patches
      while [i < stop_condition] [
        let information_item item i memory
        let this_location item 1 information_item

        if member? this_location needs_locations_list [
           set needs_locations_list remove this_location needs_locations_list
          ]
       set i i + 1
       ]
    ifelse length needs_locations_list != 0 [

        ifelse random 1 = 0 [
          move-to one-of needs_locations_list]
        [move-to one-of other patches in-radius 2 ]
        ] [
        move-to one-of patches
        print "I know all needs, I move to a random location"
      ]

    set need-here [needs] of patch-here
    set assessed-need-here need-here * capacity-pm
    let assessed-activity-here [activities] of patch-here

    let which_patch-here patch-here
    set last-assessment new-information which_patch-here ticks assessed-need-here "unknown"
    ;set last-assessment new-information which_patch-here ticks assessed-need-here assessed-activity-here ; swithced off 21-12-2018
    set recently-self-assessed lput last-assessment recently-self-assessed
    set recently-self-assessed_for_pms lput last-assessment recently-self-assessed_for_pms
    set memory lput last-assessment memory
   ]
  ]
end


to exchange-information-pm-pm
  ; exchange information in a social network
  ; PMs share information with PMs in their network to exchange information if the locations are different.
  if social-sharing = True [
    ask PMs [set information_exchange [ ]]
  ask PMs [
  if ticks > 0 [
      if network != 0 [
        ask network [
            if length recently-self-assessed_for_pms != 0 [
              ifelse [organisation] of self = [organisation] of myself
            [let random_100 random 100
              if random_100 < willingness_to_share_intra-org [

              set i 0
          set stop_condition length recently-self-assessed_for_pms - 1
          while [i < stop_condition] [
          let pm_information_item item i recently-self-assessed_for_pms
          let pm_item_id item 0 pm_information_item
          let pm_location item 1 pm_information_item
          let pm_tick item 2 pm_information_item
          let pm_who item 3 pm_information_item
          let pm_need item 4 pm_information_item
          let pm_activity item 5 pm_information_item


            let my_recently_self_assessed [recently-self-assessed_for_pms] of myself
            set j 0
            set stop_condition2 length my_recently_self_assessed - 1

            while [j < stop_condition2] [
              let my_information_item item j my_recently_self_assessed
              let my_item_id item 0 my_information_item
              let my_location item 1 my_information_item
              let my_tick item 2 my_information_item
              let my_who item 3 my_information_item
              let my_need item 4 my_information_item
              let my_activity item 5 my_information_item

              if pm_location != my_location [

                set information_exchange fput pm_information_item information_exchange ; network, the other, sets information exchange.
               ]
              set j j + 1
            ]
            set i i + 1
          ]
        ]

              ] ;intra-willigness
            [let random_100 random 100
              if random_100 < willingness_to_share_inter-org [

                set i 0
          set stop_condition length recently-self-assessed_for_pms - 1
          while [i < stop_condition] [
          let pm_information_item item i recently-self-assessed_for_pms
          let pm_item_id item 0 pm_information_item
          let pm_location item 1 pm_information_item
          let pm_tick item 2 pm_information_item
          let pm_who item 3 pm_information_item
          let pm_need item 4 pm_information_item
          let pm_activity item 5 pm_information_item


            let my_recently_self_assessed [recently-self-assessed_for_pms] of myself
            set j 0
            set stop_condition2 length my_recently_self_assessed - 1

            while [j < stop_condition2] [
              let my_information_item item j my_recently_self_assessed
              let my_item_id item 0 my_information_item
              let my_location item 1 my_information_item
              let my_tick item 2 my_information_item
              let my_who item 3 my_information_item
              let my_need item 4 my_information_item
              let my_activity item 5 my_information_item

              if pm_location != my_location [

                set information_exchange fput pm_information_item information_exchange ; network, the other, sets information exchange.
               ]
              set j j + 1
            ]
            set i i + 1
            ]
           ]
          ]
         ]
        ]
       ]
      ]

       if not empty? information_exchange [

        set i 0
        set stop_condition length information_exchange - 1
          while [i < stop_condition] [
          let information_item item i information_exchange

            ifelse not member? information_item memory [

            set memory fput information_item memory
            set information_diffused_by_pms information_diffused_by_pms + 1
            ][;show "I already know that:" show information_item
            ]
            set i i + 1
          ]
          set information_exchange [ ]
        ]
       ]
     ]



end

to exchange-information-pm-im
  ; Half way down the reporiting cycle IM requests infromation from PMs
  ; PMs share what activities they started in the past period.
  ; These are added to the general W3 list.
  if im-sharing = true [
  ask PMs [
  if ticks > 0 [
    let moment_of_request (ticks + (length_IM_reporting_cycle / 2))
    if remainder moment_of_request length_IM_reporting_cycle = 0 [
      output-write "Please send your 3W information"
      output-print ticks
      set information_exchange_W3 [ ]
  ask IMs [
    ask colleagues [
          let random_100 random 100
          if random_100  < willingness_to_share_intra-org [
          set information_exchange_W3 sentence my-activities information_exchange_W3 ]
          set my-activities [ ] ;

    ]
    if not empty? information_exchange_W3 [
    set W3 sentence information_exchange_W3 W3
        ]
  ]
  ]
]
  ]
  ]
end

to publish-w3
  ; Before the 3W can be published. It needs to be cleaned.
  ; If there is already information about a location in the 3w list. Only the newest information is kept.
  if im-sharing = true [
  if ticks >= length_IM_reporting_cycle [
  if remainder ticks length_IM_reporting_cycle = 0 [
      output-write "The is a new W3"
      output-print ticks
  ; clean W3
      set W3_clean W3
 ask IMs [
      set i 0
      set stop_condition length W3 - 1
      while [i < stop_condition] [
        let W3_information_item item i W3
        let W3_item_id item 0 W3_information_item
        let W3_location item 1 W3_information_item
        let W3_tick item 2 W3_information_item
        let W3_who item 3 W3_information_item
        let w3_need item 4 W3_information_item


        let other_W3_items remove W3_information_item W3 ;laat lijst_overige_information_item lijst van 0 t / m lengte w3, verwijder het huidge item
        let stop_condiation2 length other_W3_items - 1
        set j 0

        while [j < stop_condiation2] [
           let W3_information_item_other item j other_W3_items ; or W3  ?
           let W3_item_id_other item 0 W3_information_item_other
           let W3_location_other item 1 W3_information_item_other
           let W3_tick_other item 2 W3_information_item_other
           let W3_who_other item 3 W3_information_item_other
           let w3_need_other item 4 W3_information_item_other

           ifelse W3_location = W3_location_other [
            ifelse W3_tick > W3_tick_other [

              set W3_clean remove W3_information_item_other W3_clean ; otherwise W3_information_item_other could be put on the W3_clean an earlier time
              set j j + 1
            ] [

               set j j + 1
              ]
          ][
              set j j + 1
            ]
         ]
          set i i + 1 ]
          set W3_clean remove-duplicates W3_clean
    ; publish W3
  ]
      set W3 [ ]
      ask pms [
            ;show W3_clean
            if length W3_clean != 0 [
            let random_100 random 100
            if random_100  < willingness_to_share_inter-org [
            set memory sentence W3_clean memory
            set information_diffused_by_ims information_diffused_by_ims + length W3_clean
            ]
           ]
          ]
       show "there is a new W3, its length is"
       show length w3_clean
    ]
   ]
  ]
end


to forget-information
  ; PMs forget information in their memory lists after x number of ticks.
   ask pms [
    if ticks > 1 [
    set i 0
    set stop_condition length memory  - 1
    while [i < stop_condition ] [ ;
        ;show memory
      let information_item item i memory
        ; show information_item
      ; show information_item
      let item_tick item 2 information_item
      if ticks - item_tick >= forget-after-n-ticks [
      set memory remove-item i memory
      ;Type "I forgot"
      ;Show information_item
      set stop_condition length memory  - 1 ; adjust the stop condition to the shorter memory
      ]
     set i i + 1
    ]
    set i 0
    set stop_condition length recently-self-assessed_for_pms  - 1

    while [i < stop_condition ] [ ;

      let information_item item i recently-self-assessed_for_pms
      let item_tick item 2 information_item
      if ticks - item_tick >= forget-after-n-ticks [
      set recently-self-assessed_for_pms remove-item i recently-self-assessed_for_pms
      set stop_condition length recently-self-assessed_for_pms  - 1 ; adjust the stop condition to the shorter memory
      ]
     set i i + 1
      ]
      set i 0
      set stop_condition length recently-self-assessed  - 1

      while [i < stop_condition ] [ ;
      let information_item item i recently-self-assessed
      let item_tick item 2 information_item
      if ticks - item_tick >= forget-after-n-ticks [
       set recently-self-assessed remove-item i recently-self-assessed
       set stop_condition length recently-self-assessed  - 1 ; adjust the stop condition to the shorter memory
      ]
     set i i + 1
      ]

      set i 0
    set stop_condition length my-activities  - 1

    while [i < stop_condition ] [ ;
      let information_item item i my-activities
      let item_tick item 2 information_item
      if ticks - item_tick >= forget-after-n-ticks [
       set my-activities remove-item i my-activities
       set stop_condition length my-activities  - 1 ; adjust the stop condition to the shorter memory
       ]
     set i i + 1
      ]
     ]
    ]
end

to make-connection ;
  ;PMs make new connections every x ticks.
  if extend-network? = true [
  if ticks > 0 [
  if remainder ticks extend-network-every-n-ticks = 0 [

  ask PMs [
  let old-node find-new-partner
    if old-node != nobody
      [ create-active-link-with old-node [ set color green ]
      ]
     ]
    ]
   ]
  ]
end

to shock
  ; needs change every tick. Needs distribution is voorgesteld as een normale distributie met mean people in need and sd 10.
  ; Color of patches follows their relative gaps
  ; At a shock random 10 random places are selected with high needs. How further from the shock how lower the need.
  ;
  if item ticks disaster = 1 [
    output-print ("There is a shock!")
    show ("There is a shock")
    if random-location-shocks? [random-seed(new-seed)] ; to do: check whether the statement if location is random moment of shocks is also random. Is true.
  ask n-of epicenters patches [
    set is-epicenters? true
  ]
  ask patches [
    if is-epicenters? [
    set pcolor red
    set needs needs + round (random-normal (people-in-need) (people-in-need / 10 )) ; discuss why this distribution ; to do remove 10 as hard coded (maybe in setup, but not here).
    set is-epicenters? false
    ]
    ]
  ]
end

to connect-to-colleagues
  ;; if there are other PMs from my organisation make them colleagues and make a connection with them
  ; note this way of making connection does not follow the power law / scale free network principal
  if connect-to-colleagues? = True [
  if ticks > 0 [
  if remainder ticks extend-network-every-n-ticks  = 0 [
    ask PMs [
     if any? PMs with [organisation = [organisation] of myself] [
       set colleagues other PMs with [organisation = [organisation] of myself]
     ]
          if any? colleagues [
     ask one-of colleagues [create-active-link-with myself]
      ]
     ]
    ]
   ]
  ]
end

to hand-over-policy
  ;; PMs can hand over the knowledge they have or the contacts they have (in their network) at the end of their deployement.
  ;; How much information is handed over depends on the deployment length of their succesor.
  ;; If PMs stay longer, their collegues will invest more heavily in them.
  ask pms [
  if die-at-age - age <= 1 [

   ifelse count colleagues != 0 [
   set successor one-of colleagues
   if hand-over-knowledge = True
   [ask successor [ ;; or set memory sentence n-of round ((mean_deployment_length / 180) * length [memory] of myself ) [memory] of myself memory??
          set memory sentence n-of round ((mean_deployment_length / 300) * length [memory] of myself ) [memory] of myself memory ; assume you hand over 40% of all knowlege to do verwerk ergens (niet model)
          ]
        ]
  if hand-over-contacts = True
        [ask successor [
          create-active-links-with other n-of ((mean_deployment_length / 300) * count [network] of myself) [network] of myself [set color orange] ;; should this be replaced by not the mean (of the deployment length?)
          ]
        ]
      ]
      [print "I have no collegues for a hand-over, so I just leave"]
  ]
 ]
end

to leave-disaster
  ; PM die / leave the disaster as their deployment length is over.
  ; if the departing pms was a requester of remote support this role is carried over to a random other pm.
  ; if stay connected is true, new relations are created to prevent that there are PMS with 0 connections.
  ask PMs [
    if age  >= die-at-age
    [
      set number-undeployed number-undeployed + 1
      if requester_of_remote_support = True [
        ifelse any? colleagues [
          ask one-of colleagues [set requester_of_remote_support True] ]
        [ask one-of other pms [set requester_of_remote_support True]
          ]
      ]
      if stay-connected? = True [ ;
    if count network = 0 [
      create-active-link-with find-new-partner [ set color green ] ;
    ]
      ]
      if stay-connected? = True [
        ask other pms [
        if count network = 1 [
            if n-of 1 network = myself [
        create-active-link-with find-new-partner [ set color green ] ;
        set organisation one-of organisations
           ]
          ]
        ]
      ]
      die
    ]
  ]
end



to request-remote-support
  ; after some of the shocks remote support starts a remote assessment.
  ; one of the pms is the remote requester
  ;
  if rs-sharing = true [
  if ticks > 2 [
  if ticks-since-shock = 1 [
    if random (shocks / average_number_of_remote_assessments) = 0 [
  ask PMS [
  if requester_of_remote_support = True [
    set request_remote_support True
    ]
  ]
  ]
  ]
  ]
  ]
end

to perform-remote-assessment
  ; a remote assessment contains the needs of all patches.
  ; the needs in the assessment differ from the ground truth needs beceause the accuracy is not 100%.
  ; note: if two requests are send short after each other (the 2nd arrives while the 1st is not published, they are published together at the moment of the 2nd).
  if rs-sharing = true [
  if request_remote_support = True [
      ask patches [
        set capacity-rs (random-normal 100 sd-capacity-rs) ;/ 100 ;
        set remote-assessment lput new-information-patch self ticks (needs * capacity-rs) "unknown" remote-assessment ; remote assessments only assess needs not activities.
    ]
      set publish_remote_support ticks + assessment_length
      set request_remote_support False
  ]
  ]
end

to publish-remote-assessment
  ; The remote is assessment is shared with the requester.
  ; This hapens after x number of days (see above)
  if rs-sharing = true [
  if publish_remote_support = ticks [
    ask PMS [
      if requester_of_remote_support = True [
      show "Remote assessment"
      show remote-assessment
      show who
      set memory sentence remote-assessment memory ;
      set recently-self-assessed sentence remote-assessment recently-self-assessed ; remote-assessment is also shared with network
      set information_diffused_by_rs information_diffused_by_rs + length remote-assessment
      show "length"
      show length remote-assessment
      ;show memory

  ]
    ]
  set remote-assessment [ ]
    ]
  ]

end

to run-operations
  ; Very n (= length_activitiy) ticks, PMs have time to start a new activity.
  ; The PMs start an acitivty at location of the patch that, according to their information, has the largest gap.
  ; If there is already an activity that started there this tick the 2nd biggest gap is chosen and so on.
  ; If an activity is started, information about this activity is updated in the memory, my-activitiy and recently-self-assessed lists of the PM.
  ; this procedure can be improved.
  ask pms [
   if remainder ticks length_activity = 0 [
  if length memory != 0 [
      if biggest-gap memory != "Max is 0" [

      let biggest-gap-location item 0 biggest-gap memory
      move-to biggest-gap-location

      if [gap-patches] of patch-here > 0 [
      ask patch-here [
      set activities (size_activity / 100) * [needs] of self ; Future work: make acitivity size dependent on information
      set activity-started-this-tick true
    ]
    set my-activities fput new-information (biggest-gap-location) (ticks) ([needs] of patch-here * capacity-pm) ([activities] of patch-here) my-activities

     ;Verwijder oud informatie item uit recently self assessed, recently self assessed pm and memory.
     ;Vervang door information item inclusief activity.

    ;update memory
    set i 0
    set stop_condition length memory - 1
          while [i < stop_condition] [
           let information_item item i memory
           let location_information_item item 1 information_item

           if location_information_item = biggest-gap-location
            [set memory remove-item i memory
             set memory fput new-information (biggest-gap-location) (ticks) ([needs] of patch-here * capacity-pm) ([activities] of patch-here) memory
            ]
            set i i + 1
          ]

             ;update my-activities
    set i 0
    set stop_condition length my-activities - 1
          while [i < stop_condition] [
           let information_item item i my-activities
           let location_information_item item 1 information_item

           if location_information_item = biggest-gap-location
            [set my-activities remove-item i my-activities
             set my-activities fput new-information (biggest-gap-location) (ticks) ([needs] of patch-here * capacity-pm) ([activities] of patch-here) my-activities
            ]
            set i i + 1
          ]

          ;update recently-self-assessed
    set i 0
    set stop_condition length recently-self-assessed - 1
          while [i < stop_condition] [
           let information_item item i recently-self-assessed
           let location_information_item item 1 information_item

           if location_information_item = biggest-gap-location
            [set recently-self-assessed remove-item i recently-self-assessed
             set recently-self-assessed fput new-information (biggest-gap-location) (ticks) ([needs] of patch-here * capacity-pm) ([activities] of patch-here) recently-self-assessed
            ]
            set i i + 1
          ]

                  ;update recently-self-assessed_for_pms
    set i 0
    set stop_condition length recently-self-assessed_for_pms - 1
          while [i < stop_condition] [
           let information_item item i recently-self-assessed_for_pms
           let location_information_item item 1 information_item

           if location_information_item = biggest-gap-location
            [set recently-self-assessed_for_pms remove-item i recently-self-assessed_for_pms
             set recently-self-assessed_for_pms fput new-information (biggest-gap-location) (ticks) ([needs] of patch-here * capacity-pm) ([activities] of patch-here) recently-self-assessed_for_pms
            ]
            set i i + 1
          ]
        ]
       ]
      ]
     ]
    ]

end

to diffuse-needs
  ; Needs and activities diffuse.
  ; they spread to neighboring pathces.
  diffuse needs need-diffuse-coefficient
  diffuse activities need-diffuse-coefficient
  update-patches
end

;;;;;;;;;;;;;;;;;;;;;;;
;;;     Updates     ;;;
;;;;;;;;;;;;;;;;;;;;;;;


to update-globals
; update the variables
  set max-need-patches max [needs] of patches
  set min-need-patches min [needs] of patches
  set max-gap-patches max [gap-patches] of patches
  set min-gap-patches min [gap-patches] of patches

  set total_needs sum [needs] of patches
  set total_activities sum [activities] of patches
  set total_gap sum [gap-patches] of patches ;
  set total_redundancies sum [redundancies_patch] of patches
  set total_information_diffused information_diffused_by_pms + information_diffused_by_ims ; to do: + information_diffused_by_rs
  set total_gap_over_time total_gap_over_time + total_gap ;
  set days_worked days_worked + count pms
  set mean_deployment_length share-international-local * mean_deployment_length_local + (1 - share-international-local) * mean_deployment_length_inter
end

to update-pms
; update programme managers
  ask PMs [
  set network in-link-neighbors ; pms links not myself
  set age (age + 1)
  if any? PMs with [organisation = [organisation] of myself] [
     set colleagues other PMs with [organisation = [organisation] of myself]
     ]
  ]

end

to update-ims
  ; update information management
  ask IMs [
  set colleagues PMs with [organisation = [organisation] of myself]
  ]
end

to update-visuals
  ; update visuals
  ask PMs [ update-node-appearance ]
  ifelse show-PMs? [ask PMs [set hidden? false]][ask PMs [set hidden? True]]
  ifelse show-connections? [ask active-links [set hidden? false]][ask active-links [set hidden? True]]
  ask PMs [if requester_of_remote_support = True [set size 2 set color red]]


end

to update-patches
  ; update patches

ask patches [
set gap-patches needs - activities
set pcolor scale-color red (gap-patches) min-gap-patches (max-gap-patches + 0.01) ; To do min-gap-patches over time (evt. met switcher relatvie to gap-max now or gap-max over time)
    if activities > needs [
    set redundancies_patch activities - needs]
set activity-started-this-tick  False
    if gap-patches < 0 [set pcolor blue]
]

end

to update-node-appearance ; node procedure - to do
  ask PMs [set color scale-color orange (position organisation organisations) -5 length organisations]
end


to update-reporters
  report-disaster
end

to report-disaster ; detail: should be to-report disaster instead of report-disaster
  ; update how long it takes until a new shock etc for interface
  if length disaster != ticks [ ; only if there are ticks left
  ifelse item ticks disaster = 1 [
    set ticks-since-shock 0
    set ticks-to-shock 1

    if counter != (disaster-length - 1) [
    set counter (counter + 1)


  while [item counter disaster != 1] [
    set ticks-to-shock (ticks-to-shock + 1)
    ifelse counter != (disaster-length - 1) [
    set counter (counter + 1)
        ][stop]
  ]
      ]
    ]
      [
    set ticks-since-shock (ticks-since-shock + 1)
    set ticks-to-shock (ticks-to-shock - 1)
  ]
  ]


  output-write "Ticks pasted since last shock (or beginning): "
  output-print ticks-since-shock
  output-write "Ticks to next shock (or end): "
  output-print ticks-to-shock
end

to-report find-partner ;;
  ; find a random new partner.
  ; PMs with more links get more connections following the power law principle of social networks.
  report [one-of both-ends] of one-of links
end

to-report find-new-partner ;;
  ; find a random new partner.
  ; PMs with more links get more connections following the power law principle of social networks.
  ; if there are no links a random partner is chosen
  ifelse any? links [
  set new-partner [one-of both-ends] of one-of links
  if new-partner = self [ ;; turtle specific
  set new-partner find-new-partner]
  report new-partner
  ][set new-partner one-of other pms
    show "chosing random new partner"
  report new-partner]
end

to-report new-information [location time-stamp need activity]
  ; create an information item
    if location != "unknown" [
        if time-stamp != "unknown" [
          set id sentence location time-stamp
        ]
      ]
  let information (list id location time-stamp who need activity)
  report information

end

to-report new-information-patch [location time-stamp need activity]
  ; create an information item for remote support
  set id sentence location time-stamp
  let information (list id location time-stamp "RS" need activity)
  report information

end

to-report gap [memory-item]
  ; report a list of locations and the relief gap at these locations.
  let gap_location item 1 memory-item
  ;let gap_tick item 2 memory-item
  ;let gap_who item 3 memory-item ; future work: combine need and activity information? pm x knows needs pm y knows activity how is this consolodated.
  let gap_need item 4 memory-item
  let gap_activity item 5 memory-item

  ifelse gap_activity = "unknown" [
    ;let gap_value "unknown"
    let gap_value gap_need
    let gap-information (list gap_location gap_value)
    report gap-information]
    [
    let gap_value gap_need - gap_activity
    let gap-information (list gap_location gap_value)
    report gap-information]
end

to-report biggest-gap [memory-pm]
  ; report the biggest relief gap and the corresponding location.
  ; If there is already an acitivity being started at this location execture this procedure again (recursively).
  set i 0
  set stop_condition length memory-pm - 1
  set max_gap_value 0

  while [i < stop_condition] [
      let information_item item i memory-pm

      ifelse item 1 gap information_item = 0 [

      set i i + 1
      ][
      if item 1 gap information_item >  max_gap_value [
        set max_gap_value item 1 gap information_item
        set max_gap_location item 0 gap information_item ;
        set max_gap_position_list i ;
      ]
      set i i + 1]
  ]
      ifelse max_gap_value != 0 [
      ifelse [activity-started-this-tick] of max_gap_location = True [
      let memory-minus-old-max remove-item max_gap_position_list memory-pm
      report biggest-gap memory-minus-old-max
       ]
      [report (list max_gap_location max_gap_value)]
  ]
  [report "Max is 0"]
end




;;;;;;;
;;GIS;;
;;;;;;;

; Drawing point data from a shapefile, and optionally loading the
; data into turtles, if label-cities is true
to display-camps ;was cities
  ask city-labels [ die ]
  foreach gis:feature-list-of camps-dataset [ vector-feature ->
    gis:set-drawing-color scale-color green (gis:property-value vector-feature "Area_SqM") 2653614 322311
    gis:fill vector-feature 2.0

    ;[ ; a feature in a point dataset may have multiple points, so we
      ; have a list of lists of points, which is why we need to use
      ; first twice here
      let location gis:location-of (first (first (gis:vertex-lists-of vector-feature)))
      ; location will be an empty list if the point lies outside the
      ; bounds of the current NetLogo world, as defined by our current
      ; coordinate transformation
      if not empty? location [
       if show-names = True [
       create-city-labels 1
        [ set xcor item 0 location
          set ycor item 1 location
          set size 0
          set label gis:property-value vector-feature "New_Camp_N"
        ]
      ]
    ]
  ]

end


; Drawing polygon data from a shapefile, and optionally loading some
; of the data into turtles, if label-countries is true
to display-blocks ; was countries
  ask country-labels [ die ]
  gis:set-drawing-color white
  gis:draw blocks-dataset 1
  ;if label-countries
  set list_block_id [ ]
   foreach gis:feature-list-of blocks-dataset [ vector-feature ->
      ;let centroid gis:location-of gis:centroid-of vector-feature
      ; centroid will be an empty list if it lies outside the bounds
      ; of the current NetLogo world, as defined by our current GIS
      ; coordinate transformation
      ;if not empty? centroid
    set list_block_id fput gis:property-value vector-feature "Block_ID" list_block_id]
    ask patches [set block_id one-of list_block_id]
     ;ask patches [set block_id gis:property-value vector-feature "Block_ID"]
      ;[ create-country-labels 1
       ; [ set xcor item 0 centroid
       ;   set ycor item 1 centroid
       ;   set size 0
       ;   set label gis:property-value vector-feature "Block_ID"
       ; ]
      ;]
   ; ]

end
@#$#@#$#@
GRAPHICS-WINDOW
281
14
619
302
-1
-1
25.4
1
10
1
1
1
0
0
0
1
-6
6
-5
5
0
0
1
ticks
30.0

BUTTON
12
29
90
64
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
10
65
87
100
go-once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
10
103
85
137
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
282
734
487
767
sd_deployment_length
sd_deployment_length
1
100
6.0
1
1
NIL
HORIZONTAL

PLOT
288
332
448
456
Number of active links
ticks
show count active-links
0.0
1000.0
0.0
20.0
true
false
"" ""
PENS
"default" 1.0 0 -5298144 true "" "plot count active-links"

SLIDER
12
442
267
475
total-people-in-need
total-people-in-need
0
2000000
1000000.0
1000
1
people
HORIZONTAL

SLIDER
282
654
487
687
min_number_of_PMs
min_number_of_PMs
10
100
55.0
1
1
NIL
HORIZONTAL

SLIDER
12
403
266
436
epicenters
epicenters
0
10
2.0
1
1
NIL
HORIZONTAL

SLIDER
12
329
266
362
disaster-length
disaster-length
0
2000
730.0
1
1
NIL
HORIZONTAL

SLIDER
12
368
266
401
shocks
shocks
0
100
12.0
1
1
NIL
HORIZONTAL

OUTPUT
288
464
1338
518
13

SWITCH
92
103
279
136
random-disaster?
random-disaster?
0
1
-1000

SWITCH
89
138
278
171
random-location-shocks?
random-location-shocks?
0
1
-1000

SWITCH
92
27
277
60
show-connections?
show-connections?
0
1
-1000

SWITCH
93
63
277
96
show-PMs?
show-PMs?
0
1
-1000

PLOT
825
18
1078
168
Active PMs
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count PMs"

PLOT
453
332
616
456
distibrution-links
degree
# of PMs
1.0
10.0
0.0
25.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "let max-degree max [count link-neighbors] of PMs\n;plot-pen-reset  ;; erase what we plotted before\nset-plot-x-range 1 (max-degree + 1)  ;; + 1 to make room for the width of the last bar\nhistogram [count link-neighbors] of PMs"

SLIDER
287
543
490
576
sd-accuracy-pms
sd-accuracy-pms
0
10
2.0
1
1
NIL
HORIZONTAL

SLIDER
283
617
489
650
forget-after-n-ticks
forget-after-n-ticks
0
100
43.0
1
1
NIL
HORIZONTAL

SLIDER
496
640
701
673
extend-network-every-n-ticks
extend-network-every-n-ticks
0
100
14.0
2
1
NIL
HORIZONTAL

SLIDER
718
732
924
765
length_IM_reporting_cycle
length_IM_reporting_cycle
0
40
14.0
2
1
ticks
HORIZONTAL

TEXTBOX
16
310
166
328
Disaster setup\n
11
0.0
1

TEXTBOX
287
520
521
539
Programme managers setup\n
11
0.0
1

TEXTBOX
723
520
915
548
DoIA setup\n
11
0.0
1

TEXTBOX
933
527
1083
545
Strategies
11
0.0
1

SWITCH
13
664
268
697
stay-connected?
stay-connected?
0
1
-1000

SWITCH
14
702
268
735
extend-network?
extend-network?
0
1
-1000

SWITCH
14
738
267
771
connect-to-colleagues?
connect-to-colleagues?
0
1
-1000

CHOOSER
930
545
1135
590
RS_publication_method
RS_publication_method
"Time-focused" "Accuracy-focused" "Other"
1

SLIDER
930
595
1134
628
willingness_to_share_inter-org
willingness_to_share_inter-org
0
100
45.0
1
1
NIL
HORIZONTAL

SLIDER
930
637
1137
670
willingness_to_share_intra-org
willingness_to_share_intra-org
0
100
45.0
1
1
NIL
HORIZONTAL

TEXTBOX
18
524
168
542
Sharing setup\n
11
0.0
1

TEXTBOX
505
673
657
715
Remote support setup
11
0.0
1

PLOT
829
173
1338
460
Gaps
%
ticks
0.0
100.0
0.0
10.0
true
true
"" ""
PENS
"Gap" 1.0 0 -5298144 true "" "plot total_gap\n"
"Needs" 1.0 0 -13345367 true "" "plot total_needs"
"Activities" 1.0 0 -13840069 true "" "plot total_activities\n"
"0" 1.0 0 -7500403 true "plot 0" "plot 0"

PLOT
621
16
821
168
Information diffused
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total_information_diffused"
"pen-1" 1.0 0 -8330359 true "" "plot information_diffused_by_pms"
"pen-2" 1.0 0 -5325092 true "" "plot information_diffused_by_ims"
"pen-3" 1.0 0 -3508570 true "" "plot information_diffused_by_rs"

PLOT
622
333
819
457
Undeployed
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" " plot number-undeployed "

SLIDER
494
566
706
599
length_activity
length_activity
0
60
4.0
1
1
NIL
HORIZONTAL

CHOOSER
127
183
274
228
disaster-scenario
disaster-scenario
"Bangladesh" "Bangladesh-like" "Syria" "Yemen" "Iraq" "Ethiopia" "Malawi" "Nigeria" "Somalia"
1

BUTTON
12
140
87
174
NIL
load-scenario
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
13
183
110
216
scenario
scenario
0
1
-1000

SLIDER
494
603
706
636
size_activity
size_activity
0
200
100.0
1
1
% of needs at patch
HORIZONTAL

SLIDER
500
693
704
726
sd-capacity-rss
sd-capacity-rss
0
50
21.0
1
1
NIL
HORIZONTAL

SLIDER
500
733
705
766
average_number_of_remote_assessments
average_number_of_remote_assessments
0
shocks - 1
4.0
1
1
NIL
HORIZONTAL

SLIDER
282
695
488
728
mean_deployment_length
mean_deployment_length
0
180
173.20000000000002
1
1
NIL
HORIZONTAL

SLIDER
717
542
915
575
goal
goal
0
1
0.15
.01
1
NIL
HORIZONTAL

SLIDER
717
579
913
612
PMs-per-tick-per-DoIA
PMs-per-tick-per-DoIA
0
10
1.0
1
1
NIL
HORIZONTAL

MONITOR
623
169
821
214
KPI: total gaps
total_gap_over_time
17
1
11

SLIDER
13
481
268
514
need-diffuse-coefficient
need-diffuse-coefficient
0
1
0.2
0.01
1
NIL
HORIZONTAL

SWITCH
143
272
272
305
show-names
show-names
1
1
-1000

BUTTON
10
232
133
265
NIL
display-camps\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
142
232
271
267
NIL
display-blocks\n\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
9
272
137
305
show-map
show-map
0
1
-1000

SWITCH
14
550
268
583
social-sharing
social-sharing
0
1
-1000

SWITCH
13
588
268
621
IM-sharing
IM-sharing
0
1
-1000

SWITCH
13
625
268
658
RS-sharing
RS-sharing
0
1
-1000

PLOT
1080
17
1337
167
Days worked
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 false "" "plot days_worked"

MONITOR
623
259
820
304
KPI: total days worked
days_worked
17
1
11

SLIDER
717
619
913
652
deployment-frequency
deployment-frequency
0
30
17.0
1
1
NIL
HORIZONTAL

SLIDER
1144
545
1339
578
share-international-local
share-international-local
0
1
0.64
0.01
1
NIL
HORIZONTAL

SLIDER
717
659
916
692
number-of-organisations
number-of-organisations
0
10
2.0
1
1
NIL
HORIZONTAL

MONITOR
624
215
821
260
KPI: total Information diffused
total_information_diffused
17
1
11

TEXTBOX
290
315
367
338
Verification:
11
0.0
1

TEXTBOX
457
315
535
338
Verification:
11
0.0
1

TEXTBOX
623
314
703
333
Verification:
11
0.0
1

SLIDER
283
580
491
613
assessment_length
assessment_length
0
10
10.0
1
1
NIL
HORIZONTAL

SWITCH
1145
595
1339
628
hand-over-knowledge
hand-over-knowledge
1
1
-1000

SWITCH
1145
635
1339
668
hand-over-contacts
hand-over-contacts
1
1
-1000

TEXTBOX
724
709
870
735
IM setup
11
0.0
1

TEXTBOX
14
13
202
32
Model setup\n
11
0.0
1

CHOOSER
930
679
1138
724
Willingness_focus
Willingness_focus
"Not specified" "Inward" "Outward"
0

CHOOSER
495
520
705
565
distribution-accuracy
distribution-accuracy
1 2 3 4 5
0

TEXTBOX
1442
659
1630
728
1 = \"normal\"\n2 = \"normalskewedleft\"\n3 = \"binomal\"\n4 = \"binomalskewedleft\"\n5 = \"binomalskewedright\"
11
0.0
1

@#$#@#$#@
# This model is used for the master thesis research project Information diffusion in complex emergencies. More instructions can be found in the thesis uploaded here: https://repository.tudelft.nl/islandora/object/uuid%3Aad1f8abf-1a04-4679-b0d3-bf9c564ff595 

## WHAT IS IT?

This model reflects information diffusion among agents in complex emergencies. It can be used to evaluate strategies that influence information diffusion and the effectivness and efficency of relief activities.

## HOW IT WORKS

This model contains four agent types: Director of international assistance,programme manager,  information management and remote support.

The Director of international assistance deploys programme managers to the disaster site. The programme manager is deployed, conducts needs assessments, exhanges information in social networks and starts relief activities. Information management exchanges information randomly. Remote support provides assessments and shares them with one of the programme managers. Collectivly the agents produce, diffuse and use information to plan and exectue relief activities. How this happens is discussed in more detail in the thesis information diffusion in complex emergencies.

This BPMN model provides a graphical representation of how the agents interact with each other and with the environment. 

![BPMN](https://raw.githubusercontent.com/JasperCM/information-diffusion/master/conceptual-models/BPMN_small.jpg)

A larger version of this diagram can be found on Github: https://raw.githubusercontent.com/JasperCM/information-diffusion/master/conceptual-models/BPMN.jpg

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

The input variables can be grouped in 5 functional categories. These categories are model setup, disaster setup, sharing setup, agent setup and strategy setup. The first category, model setup allows the user to start and stop the model. It also gives the user control over the visualisation options as users can select to show or hide agents, connections or map projections. Furthermore, it enables the user to load in a disaster. By loading in a disaster variables in other categories are set to a pre- specified value. The ABM is constructed based on a Bangladesh case study. If the Bangladesh-like scenario is loaded all variables are set to values that, according to the researcher, represent the situation in Bangladesh.

In the disaster setup section, the user can specify the length and the number of shocks in the disaster. The variable epicentres defines the number of places at which a shock hits. If this number is set to two, a shock hits in two places simultan- eously. This represents a monsoon storm, landslide or sudden refugee influx chan- ging needs in two places at once. By changing the total people in need slider one changes the number of people that are hit over the entire disaster length. Dividing the total people in need by the number of shocks and the number of epicentres gives the number of people that are hit in one epicentre of one shock. Every day a part of the needs and relief activities diffuse from one camp to another. This can be seen asrefugees looking for relief in neighbouring camps or hygiene kits being shared of camp borders. If diffuse needs variable is set to 0.20, 20% of all needs and activities at one camp transfer to all neighbouring camps at the end of each day.

The sharing setup section gives the user control over a number of conceptual choices. As an example, s/he can switch off social sharing, as a result all information is shared only through information management and not through social networks. If RS-sharing is switched off, there is no remote support in the disaster. In case stay connected is switched off, programme managers may have zero connections and, or new, not connected social networks might emerge. Switching off extend network replicates a scenario where social networks are not extended but stay constant over time. This represents a situation where no meetings are organised, communication might be impossible and there is no possibility to get to other programme man- agers. When connect-to-colleagues is switched on programme managers connect to other programme managers from their own organisation in addition to their ’nor- mal’ network extension that is independent of their organisation.
The agent setup section sets the values for variables that are agent specific. Ex- amples are the standard deviation of the normal distribution of the accuracy of assessments carried out by programme managers or remote support. There are also variables that relate to the time it takes to do activities or assessments and to the moment at which activities or assessments will be started. Another variable sets the frequency at which directors of international assistance decide to send more programme man- agers to the field. The number of programme managers they send at the specified frequency is also a variable. Another variable belonging to the director of interna- tional assistance agent class is goal. This variable sets a target for the programme managers, if this target is reached, no additional programme managers are send to the field. Some of the agent specific variables can be influence by strategies. An ex- ample is the mean deployment length. This can be set by the user but it is overwritten if the strategy increase share of local programme managers is deployed by changing the strategy lever share international local.


The 5th and last section with variables is the strategies setup section. The vari- ables willingness to share information inter-organisational, willingness to share information intra-organisational and share international local are also referred to as strategy levers. These sliders, shown at the right side of the interface, form the nuts and bolts of the specific strategies and can be set to a multitude of different values. Setting willing- ness to share information intra-organisational to 45% means that at each moment that information is shared with a other programme managers of the same organisation, 45% of all information items known to the programme manager are shared. Increas- ing the share of local programme managers increases the mean deployment length because, as by default, the deployment length of local programme managers is longer then that of international programme managers.

Publication method, hand-over knowledge and hand-over contracts are strategy select- ors. These variables can take binary values. These are on or off in the case of the hand-over strategies and accuracy-focused or time-focused in the case of publica- tion method. Setting publication method to time-focused means the time needed to perform an assessment is short but the accuracy of the assessment is low. Accuracy-focused represents the opposite, assessments are accurate but take long to perform. If a hand-over strategy is set to on, programme managers transfer a share of the information(items) or contacts they have with someone from their organisation (their successor). How much the programme manager and successor are able to share depends on the deployment length of the successor. The hand-over of programme managers that stay longer is more effective if the programme managers stays longer, as was concluded based on interviews.

The model output can be grouped in 4 categories that all have their specific visual- isation. These are the grid or model world, the verification plots and outputs, the model behaviour plots and the KPI values. The model world is a graphical representation of the place where the agents in- teract which each other and with the environment. In this world, the programme manager is represented as a person with a red vest. If programme managers are connected by a green line, they are in contacts with each other. The group of programme managers that is connected to a specific programme manager is the social network of that programme manager. In the world, information management is represented as an office building, remote support as a computer and the director of international assistance has a briefcase to store important documents. The pixels or patches in the grid are a specific type of agent. Patches with a relatively high relief gap light up.

The three plots below the grid are verification plots. These plots can be used to see whether the model behaves as expected. The other plots in the model interface can be used to interpret the model behaviour. As an example, the gap plot shows the total needs, activities and gaps of all patches combined. Another example is the days worked plot, this plot shows the sum of the number of programme manager for each day. In other words, the plot shows the total number of working days programme managers spend in the disaster. An important aspect of the interface are the Key Performance Indicator (KPI) values, shown to the right of the grid in the middle of the model interface. In the model interface, these values are used the evaluate the model behaviour and hence the effects of the strategies. The three KPIs are total relief gap per programme manager per day, total information diffused per programme manager per day and total days worked. As is common for models of complex socio-technical systems, the behaviour of the systems can only be interpreted by looking at multiple variables at once. As an example, once information diffusion increases and the relief gap decreases one should also look at the days worked variable to check whether the effects caused by an efficiency gain or simply by the fact that more programme managers where active in the disaster.

## THINGS TO NOTICE

The disaster evolves a result of serries of shocks. These shocks differ in location and timing. As a result of the deployments and undeployments information is lossed about the disater is lost. Different strategies can be used to increase information diffusion and affect the effectivenss and efficency of the relief opperation. 

## THINGS TO TRY

Try using the information sharing strategies. Which strategies show most effective? In which situation? Also, try changing the type of disaster. Was is the effect of a disaster with a lot of shocks at the start and what of a disaster with shocks evenly distributed of the disaster. 

## EXTENDING THE MODEL

The way programme managers use information to make decisions can be improved. Currently, programme managers use the information have (stored in their memory) to determine the location/patch with the biggest differents between needs and acitivities (the largest relief gap). This code can be changed to relect the more complex ways humanitarians create mental models of their situation and make decisions based on an overload of information. In addition, in the current model the activities are based on the ground truth needs of the pathces. Changing this to the reported gaps (as reflected by the information) whould make the model more realistic. Lastly, the heterogeneity of organisations, information and infrastructure can be increased. What whould, for example, be the effect of including false information or organisations that have competing interests? 

## NETLOGO FEATURES

This model uses the GIS extension package. It is however, not required to load new GIS maps / layers into the model to change the parametrisation to reflect a different disaster. The GIS extension mainly serves visualisation purpuses. 

This model also uses the diffuse procudure of Netlogo. Needs and activities diffuse from one patch to another. 

## RELATED MODELS

The first version of this model was inspired on the Diffusion on a Directed Network model included in the model libary of Netlogo. 

## CREDITS AND REFERENCES

This model builds on the work of Altay and Pal (2014) and Bateman and Gralla (2018).

Altay, N. & Pal, R. (2014). Information diffusion among agents: Implications for humanitarian operations. Production and Operations Management.

Bateman, L. & Gralla, E. (2018). Evaluating Strategies for Intra-Organizational In- formation Management in Humanitarian Response. PLOS Medicine, 20–23.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

computer workstation
false
0
Rectangle -7500403 true true 60 45 240 180
Polygon -7500403 true true 90 180 105 195 135 195 135 210 165 210 165 195 195 195 210 180
Rectangle -16777216 true false 75 60 225 165
Rectangle -7500403 true true 45 210 255 255
Rectangle -10899396 true false 249 223 237 217
Line -16777216 false 60 225 120 225

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

house two story
false
0
Polygon -7500403 true true 2 180 227 180 152 150 32 150
Rectangle -7500403 true true 270 75 285 255
Rectangle -7500403 true true 75 135 270 255
Rectangle -16777216 true false 124 195 187 256
Rectangle -16777216 true false 210 195 255 240
Rectangle -16777216 true false 90 150 135 180
Rectangle -16777216 true false 210 150 255 180
Line -16777216 false 270 135 270 255
Rectangle -7500403 true true 15 180 75 255
Polygon -7500403 true true 60 135 285 135 240 90 105 90
Line -16777216 false 75 135 75 180
Rectangle -16777216 true false 30 195 93 240
Line -16777216 false 60 135 285 135
Line -16777216 false 255 105 285 135
Line -16777216 false 0 180 75 180
Line -7500403 true 60 195 60 240
Line -7500403 true 154 195 154 255

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

person business
false
0
Rectangle -1 true false 120 90 180 180
Polygon -13345367 true false 135 90 150 105 135 180 150 195 165 180 150 105 165 90
Polygon -7500403 true true 120 90 105 90 60 195 90 210 116 154 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 183 153 210 210 240 195 195 90 180 90 150 165
Circle -7500403 true true 110 5 80
Rectangle -7500403 true true 127 76 172 91
Line -16777216 false 172 90 161 94
Line -16777216 false 128 90 139 94
Polygon -13345367 true false 195 225 195 300 270 270 270 195
Rectangle -13791810 true false 180 225 195 300
Polygon -14835848 true false 180 226 195 226 270 196 255 196
Polygon -13345367 true false 209 202 209 216 244 202 243 188
Line -16777216 false 180 90 150 165
Line -16777216 false 120 90 150 165

person service
false
0
Polygon -7500403 true true 180 195 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285
Polygon -1 true false 120 90 105 90 60 195 90 210 120 150 120 195 180 195 180 150 210 210 240 195 195 90 180 90 165 105 150 165 135 105 120 90
Polygon -1 true false 123 90 149 141 177 90
Rectangle -7500403 true true 123 76 176 92
Circle -7500403 true true 110 5 80
Line -13345367 false 121 90 194 90
Line -16777216 false 148 143 150 196
Rectangle -16777216 true false 116 186 182 198
Circle -1 true false 152 143 9
Circle -1 true false 152 166 9
Rectangle -16777216 true false 179 164 183 186
Polygon -2674135 true false 180 90 195 90 183 160 180 195 150 195 150 135 180 90
Polygon -2674135 true false 120 90 105 90 114 161 120 195 150 195 150 135 120 90
Polygon -2674135 true false 155 91 128 77 128 101
Rectangle -16777216 true false 118 129 141 140
Polygon -2674135 true false 145 91 172 77 172 101

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>total_information_diffused</metric>
    <metric>count PMs</metric>
    <metric>count active-links</metric>
    <metric>number-undeployed</metric>
    <metric>total_gap</metric>
    <metric>total_activities</metric>
    <metric>total_needs</metric>
    <enumeratedValueSet variable="length_activity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_deployment_length">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="people-in-need">
      <value value="40000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connect-to-colleagues?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment_frequency">
      <value value="18"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_of_remote_assessments">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_inter-org">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forget-after-n-ticks">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epicenters">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS_publication_method">
      <value value="&quot;Periodical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_activity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-location-shocks?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-capacity-rss">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-disaster?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-scenario">
      <value value="&quot;Bangladesh-like&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-PMs?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_of_PMs">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-length">
      <value value="730"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shocks">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="variance-capacity-pms">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="mean_deployment_length" first="20" step="10" last="160"/>
    <enumeratedValueSet variable="extend-network-every-n-ticks">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-connections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_intra-org">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_IM_reporting_cycle">
      <value value="14"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>total_information_diffused</metric>
    <metric>days_worked</metric>
    <metric>total_gap_over_time</metric>
    <steppedValueSet variable="deployment-frequency" first="2" step="2" last="20"/>
    <enumeratedValueSet variable="length_activity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_number_of_PMs">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_deployment_length">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connect-to-colleagues?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment_frequency">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-people-in-need">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_inter-org">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_of_remote_assessments">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forget-after-n-ticks">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-map">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS_publication_method">
      <value value="&quot;Adaptive&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epicenters">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_activity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-location-shocks?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="goal" first="0.05" step="0.05" last="0.25"/>
    <enumeratedValueSet variable="sd-capacity-rss">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-scenario">
      <value value="&quot;Bangladesh-like&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-disaster?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS-sharing">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PMs-per-tick-per-DoIA">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-PMs?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-names">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="need-diffuse-coefficient">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-length">
      <value value="730"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shocks">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_deployment_length">
      <value value="30"/>
      <value value="60"/>
      <value value="90"/>
      <value value="120"/>
      <value value="180"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network-every-n-ticks">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-connections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IM-sharing">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-accuracy-pms">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_intra-org">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-sharing">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_IM_reporting_cycle">
      <value value="14"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>total_information_diffused</metric>
    <metric>days_worked</metric>
    <metric>total_gap_over_time</metric>
    <enumeratedValueSet variable="length_activity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deployment-frequency">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_number_of_PMs">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_deployment_length">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connect-to-colleagues?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment_frequency">
      <value value="7"/>
    </enumeratedValueSet>
    <steppedValueSet variable="willingness_to_share" first="0" step="10" last="100"/>
    <enumeratedValueSet variable="total-people-in-need">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_inter-org">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_of_remote_assessments">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forget-after-n-ticks">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epicenters">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-map">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS_publication_method">
      <value value="&quot;Adaptive&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_activity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-location-shocks?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="goal">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-capacity-rss">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-scenario">
      <value value="&quot;Bangladesh-like&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-disaster?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PMs-per-tick-per-DoIA">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-PMs?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-names">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-length">
      <value value="730"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="need-diffuse-coefficient">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shocks">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_deployment_length">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-connections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network-every-n-ticks">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IM-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-accuracy-pms">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_intra-org">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_IM_reporting_cycle">
      <value value="14"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="2000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>total_information_diffused</metric>
    <metric>days_worked</metric>
    <metric>total_gap_over_time</metric>
    <enumeratedValueSet variable="length_activity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deployment-frequency">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_number_of_PMs">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_deployment_length">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connect-to-colleagues?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment_frequency">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-people-in-need">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_inter-org">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_of_remote_assessments">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forget-after-n-ticks">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epicenters">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-map">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS_publication_method">
      <value value="&quot;Adaptive&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_activity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-location-shocks?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="goal">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-capacity-rss">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-scenario">
      <value value="&quot;Bangladesh-like&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-disaster?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PMs-per-tick-per-DoIA">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-PMs?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-names">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-length">
      <value value="730"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="need-diffuse-coefficient">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shocks">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="mean_deployment_length" first="60" step="10" last="180"/>
    <enumeratedValueSet variable="show-connections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network-every-n-ticks">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IM-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-accuracy-pms">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_intra-org">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_IM_reporting_cycle">
      <value value="14"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>total_information_diffused</metric>
    <metric>days_worked</metric>
    <metric>total_gap_over_time</metric>
    <metric>disaster</metric>
    <enumeratedValueSet variable="deployment-frequency">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_activity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_number_of_PMs">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_deployment_length">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connect-to-colleagues?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment_frequency">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-people-in-need">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_inter-org">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number_of_remote_assessments">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forget-after-n-ticks">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS_publication_method">
      <value value="&quot;Periodical&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-map">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epicenters">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_activity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-location-shocks?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="goal">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-capacity-rss">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-scenario">
      <value value="&quot;Bangladesh-like&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PMs-per-tick-per-DoIA">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-disaster?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-PMs?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-names">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-length">
      <value value="730"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="need-diffuse-coefficient">
      <value value="0.2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="shocks" first="2" step="2" last="100"/>
    <enumeratedValueSet variable="extend-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_deployment_length">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-connections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network-every-n-ticks">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IM-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-accuracy-pms">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_intra-org">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_IM_reporting_cycle">
      <value value="14"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="deployment-frequency">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_activity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_number_of_PMs">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_deployment_length">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connect-to-colleagues?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average_number_of_remote_assessments">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-people-in-need">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_inter-org">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forget-after-n-ticks">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epicenters">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS_publication_method">
      <value value="&quot;Time-focused&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-map">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_activity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-location-shocks?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="goal">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-capacity-rss">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-disaster?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-scenario">
      <value value="&quot;Bangladesh-like&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PMs-per-tick-per-DoIA">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-PMs?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-names">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-length">
      <value value="730"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="share-international-local">
      <value value="0.64"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="need-diffuse-coefficient">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shocks">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment_length">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-knowledge">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_deployment_length">
      <value value="147.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network-every-n-ticks">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-connections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-contacts">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IM-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stay-connected?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-accuracy-pms">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_intra-org">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_IM_reporting_cycle">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-organisations">
      <value value="2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Policy_experiment" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="750"/>
    <metric>total_information_diffused</metric>
    <metric>days_worked</metric>
    <metric>total_gap_over_time</metric>
    <metric>disaster</metric>
    <metric>information_diffused_by_pms</metric>
    <metric>information_diffused_by_ims</metric>
    <metric>information_diffused_by_rs</metric>
    <enumeratedValueSet variable="deployment-frequency">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_activity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_number_of_PMs">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_deployment_length">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connect-to-colleagues?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average_number_of_remote_assessments">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-people-in-need">
      <value value="1000000"/>
    </enumeratedValueSet>
    <steppedValueSet variable="willingness_to_share_inter-org" first="0" step="20" last="100"/>
    <enumeratedValueSet variable="forget-after-n-ticks">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epicenters">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS_publication_method">
      <value value="&quot;Time-focused&quot;"/>
      <value value="&quot;Accuracy-focused&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-map">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_activity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-location-shocks?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="goal">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-capacity-rss">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-disaster?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-scenario">
      <value value="&quot;Bangladesh-like&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PMs-per-tick-per-DoIA">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-PMs?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-names">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-length">
      <value value="730"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="share-international-local">
      <value value="0.4"/>
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="need-diffuse-coefficient">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shocks">
      <value value="6"/>
      <value value="12"/>
      <value value="24"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment_length">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-knowledge">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_deployment_length">
      <value value="147.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network-every-n-ticks">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-connections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-contacts">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IM-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stay-connected?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-accuracy-pms">
      <value value="8"/>
    </enumeratedValueSet>
    <steppedValueSet variable="willingness_to_share_intra-org" first="0" step="20" last="100"/>
    <enumeratedValueSet variable="social-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_IM_reporting_cycle">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-organisations">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Policy_experiment_willingness_inter" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="750"/>
    <metric>total_information_diffused</metric>
    <metric>days_worked</metric>
    <metric>total_gap_over_time</metric>
    <metric>disaster</metric>
    <metric>information_diffused_by_pms</metric>
    <metric>information_diffused_by_ims</metric>
    <metric>information_diffused_by_rs</metric>
    <enumeratedValueSet variable="deployment-frequency">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_activity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_number_of_PMs">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_deployment_length">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connect-to-colleagues?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average_number_of_remote_assessments">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-people-in-need">
      <value value="1000000"/>
    </enumeratedValueSet>
    <steppedValueSet variable="willingness_to_share_inter-org" first="0" step="10" last="80"/>
    <enumeratedValueSet variable="forget-after-n-ticks">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epicenters">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS_publication_method">
      <value value="&quot;Accuracy-focused&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-map">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_activity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-location-shocks?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="goal">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-capacity-rss">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-disaster?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-scenario">
      <value value="&quot;Bangladesh-like&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PMs-per-tick-per-DoIA">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-PMs?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-names">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-length">
      <value value="730"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="share-international-local">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="need-diffuse-coefficient">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shocks">
      <value value="6"/>
      <value value="12"/>
      <value value="18"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment_length">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-knowledge">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_deployment_length">
      <value value="147.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network-every-n-ticks">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-connections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-contacts">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IM-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stay-connected?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-accuracy-pms">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_intra-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_IM_reporting_cycle">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-organisations">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Policy_experiment_willingness_intra" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="750"/>
    <metric>total_information_diffused</metric>
    <metric>days_worked</metric>
    <metric>total_gap_over_time</metric>
    <metric>information_diffused_by_pms</metric>
    <metric>information_diffused_by_ims</metric>
    <metric>information_diffused_by_rs</metric>
    <enumeratedValueSet variable="deployment-frequency">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_activity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_number_of_PMs">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_deployment_length">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connect-to-colleagues?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average_number_of_remote_assessments">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-people-in-need">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_inter-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forget-after-n-ticks">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epicenters">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS_publication_method">
      <value value="&quot;Accuracy-focused&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-map">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_activity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-location-shocks?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="goal">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-capacity-rss">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-disaster?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-scenario">
      <value value="&quot;Bangladesh-like&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PMs-per-tick-per-DoIA">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-PMs?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-names">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-length">
      <value value="730"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="share-international-local">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="need-diffuse-coefficient">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shocks">
      <value value="6"/>
      <value value="12"/>
      <value value="18"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment_length">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-knowledge">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_deployment_length">
      <value value="147.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network-every-n-ticks">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-connections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-contacts">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IM-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stay-connected?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-accuracy-pms">
      <value value="8"/>
    </enumeratedValueSet>
    <steppedValueSet variable="willingness_to_share_intra-org" first="0" step="10" last="80"/>
    <enumeratedValueSet variable="social-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_IM_reporting_cycle">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-organisations">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Policy_experiment_share_local_int" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="750"/>
    <metric>total_information_diffused</metric>
    <metric>days_worked</metric>
    <metric>total_gap_over_time</metric>
    <metric>information_diffused_by_pms</metric>
    <metric>information_diffused_by_ims</metric>
    <metric>information_diffused_by_rs</metric>
    <enumeratedValueSet variable="deployment-frequency">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_activity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_number_of_PMs">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_deployment_length">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connect-to-colleagues?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average_number_of_remote_assessments">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-people-in-need">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_inter-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forget-after-n-ticks">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epicenters">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS_publication_method">
      <value value="&quot;Accuracy-focused&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-map">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_activity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-location-shocks?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="goal">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-capacity-rss">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-disaster?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-scenario">
      <value value="&quot;Bangladesh-like&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PMs-per-tick-per-DoIA">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-PMs?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-names">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-length">
      <value value="730"/>
    </enumeratedValueSet>
    <steppedValueSet variable="share-international-local" first="0.4" step="0.1" last="0.8"/>
    <enumeratedValueSet variable="need-diffuse-coefficient">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shocks">
      <value value="6"/>
      <value value="12"/>
      <value value="18"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment_length">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-knowledge">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_deployment_length">
      <value value="147.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network-every-n-ticks">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-connections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-contacts">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IM-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stay-connected?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-accuracy-pms">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_intra-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_IM_reporting_cycle">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-organisations">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Policy_experiment_share_method" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="750"/>
    <metric>total_information_diffused</metric>
    <metric>days_worked</metric>
    <metric>total_gap_over_time</metric>
    <metric>information_diffused_by_pms</metric>
    <metric>information_diffused_by_ims</metric>
    <metric>information_diffused_by_rs</metric>
    <enumeratedValueSet variable="deployment-frequency">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_activity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_number_of_PMs">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_deployment_length">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connect-to-colleagues?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average_number_of_remote_assessments">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-people-in-need">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_inter-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forget-after-n-ticks">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epicenters">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS_publication_method">
      <value value="&quot;Accuracy-focused&quot;"/>
      <value value="&quot;Time-focused&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-map">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_activity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-location-shocks?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="goal">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-capacity-rss">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-disaster?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-scenario">
      <value value="&quot;Bangladesh-like&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PMs-per-tick-per-DoIA">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-PMs?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-names">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-length">
      <value value="730"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="share-international-local">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="need-diffuse-coefficient">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shocks">
      <value value="6"/>
      <value value="12"/>
      <value value="18"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment_length">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-knowledge">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_deployment_length">
      <value value="147.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network-every-n-ticks">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-connections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-contacts">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IM-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stay-connected?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-accuracy-pms">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_intra-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_IM_reporting_cycle">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-organisations">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Policy_experiment_hand_over_contacts" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="750"/>
    <metric>total_information_diffused</metric>
    <metric>days_worked</metric>
    <metric>total_gap_over_time</metric>
    <metric>information_diffused_by_pms</metric>
    <metric>information_diffused_by_ims</metric>
    <metric>information_diffused_by_rs</metric>
    <enumeratedValueSet variable="deployment-frequency">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_activity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_number_of_PMs">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_deployment_length">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connect-to-colleagues?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average_number_of_remote_assessments">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-people-in-need">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_inter-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forget-after-n-ticks">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epicenters">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS_publication_method">
      <value value="&quot;Accuracy-focused&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-map">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_activity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-location-shocks?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="goal">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-capacity-rss">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-disaster?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-scenario">
      <value value="&quot;Bangladesh-like&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PMs-per-tick-per-DoIA">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-PMs?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-names">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-length">
      <value value="730"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="share-international-local">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="need-diffuse-coefficient">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shocks">
      <value value="6"/>
      <value value="12"/>
      <value value="18"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment_length">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-knowledge">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_deployment_length">
      <value value="147.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network-every-n-ticks">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-connections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-contacts">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IM-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stay-connected?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-accuracy-pms">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_intra-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_IM_reporting_cycle">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-organisations">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Policy_experiment_hand_over_knowledge" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="750"/>
    <metric>total_information_diffused</metric>
    <metric>days_worked</metric>
    <metric>total_gap_over_time</metric>
    <metric>information_diffused_by_pms</metric>
    <metric>information_diffused_by_ims</metric>
    <metric>information_diffused_by_rs</metric>
    <enumeratedValueSet variable="deployment-frequency">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_activity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_number_of_PMs">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_deployment_length">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connect-to-colleagues?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average_number_of_remote_assessments">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-people-in-need">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_inter-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forget-after-n-ticks">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epicenters">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS_publication_method">
      <value value="&quot;Accuracy-focused&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-map">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_activity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-location-shocks?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="goal">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-capacity-rss">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-disaster?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-scenario">
      <value value="&quot;Bangladesh-like&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PMs-per-tick-per-DoIA">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-PMs?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-names">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-length">
      <value value="730"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="share-international-local">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="need-diffuse-coefficient">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shocks">
      <value value="6"/>
      <value value="12"/>
      <value value="18"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment_length">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-knowledge">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_deployment_length">
      <value value="147.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network-every-n-ticks">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-connections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-contacts">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IM-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stay-connected?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-accuracy-pms">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_intra-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_IM_reporting_cycle">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-organisations">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Policy_experiment_effect_distributions" repetitions="3" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="750"/>
    <metric>total_information_diffused</metric>
    <metric>days_worked</metric>
    <metric>total_gap_over_time</metric>
    <metric>information_diffused_by_pms</metric>
    <metric>information_diffused_by_ims</metric>
    <metric>information_diffused_by_rs</metric>
    <enumeratedValueSet variable="deployment-frequency">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_activity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_number_of_PMs">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_deployment_length">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connect-to-colleagues?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average_number_of_remote_assessments">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-people-in-need">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_inter-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forget-after-n-ticks">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epicenters">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS_publication_method">
      <value value="&quot;Accuracy-focused&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-map">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_activity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-location-shocks?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="goal">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-capacity-rss">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-disaster?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-scenario">
      <value value="&quot;Bangladesh-like&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PMs-per-tick-per-DoIA">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-PMs?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-names">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-length">
      <value value="730"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="share-international-local">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="need-diffuse-coefficient">
      <value value="0.2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="shocks" first="2" step="2" last="40"/>
    <enumeratedValueSet variable="assessment_length">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-knowledge">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_deployment_length">
      <value value="147.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network-every-n-ticks">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-connections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-contacts">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IM-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stay-connected?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-accuracy-pms">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_intra-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_IM_reporting_cycle">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-organisations">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-accuracy">
      <value value="1"/>
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Policy_experiment_effect_moment_shock" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="750"/>
    <metric>total_information_diffused</metric>
    <metric>days_worked</metric>
    <metric>total_gap_over_time</metric>
    <metric>information_diffused_by_pms</metric>
    <metric>information_diffused_by_ims</metric>
    <metric>information_diffused_by_rs</metric>
    <metric>count pms</metric>
    <enumeratedValueSet variable="deployment-frequency">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_activity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_number_of_PMs">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_deployment_length">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connect-to-colleagues?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average_number_of_remote_assessments">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-people-in-need">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_inter-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forget-after-n-ticks">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epicenters">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS_publication_method">
      <value value="&quot;Accuracy-focused&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-map">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_activity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-location-shocks?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="goal">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-capacity-rss">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-disaster?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-scenario">
      <value value="&quot;Bangladesh-like&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PMs-per-tick-per-DoIA">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-PMs?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-names">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-length">
      <value value="730"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="share-international-local">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="need-diffuse-coefficient">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shocks">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment_length">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-knowledge">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_deployment_length">
      <value value="147.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network-every-n-ticks">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-connections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-contacts">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IM-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stay-connected?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-accuracy-pms">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_intra-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_IM_reporting_cycle">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-organisations">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Policy_experiment_willingness_inter_and_intra_sen_shocks_orgs" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="750"/>
    <metric>total_information_diffused</metric>
    <metric>days_worked</metric>
    <metric>total_gap_over_time</metric>
    <metric>disaster</metric>
    <metric>information_diffused_by_pms</metric>
    <metric>information_diffused_by_ims</metric>
    <metric>information_diffused_by_rs</metric>
    <enumeratedValueSet variable="deployment-frequency">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_activity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_number_of_PMs">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_deployment_length">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connect-to-colleagues?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average_number_of_remote_assessments">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-people-in-need">
      <value value="1000000"/>
    </enumeratedValueSet>
    <steppedValueSet variable="willingness_to_share_inter-org" first="0" step="10" last="80"/>
    <enumeratedValueSet variable="forget-after-n-ticks">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epicenters">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS_publication_method">
      <value value="&quot;Accuracy-focused&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-map">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_activity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-location-shocks?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="goal">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-capacity-rss">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-disaster?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-scenario">
      <value value="&quot;Bangladesh-like&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PMs-per-tick-per-DoIA">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-PMs?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-names">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-length">
      <value value="730"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="share-international-local">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="need-diffuse-coefficient">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shocks">
      <value value="6"/>
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment_length">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-knowledge">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_deployment_length">
      <value value="147.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network-every-n-ticks">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-connections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-contacts">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IM-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stay-connected?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-accuracy-pms">
      <value value="8"/>
    </enumeratedValueSet>
    <steppedValueSet variable="willingness_to_share_intra-org" first="0" step="10" last="80"/>
    <enumeratedValueSet variable="social-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_IM_reporting_cycle">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-organisations">
      <value value="4"/>
      <value value="8"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Policy_experiment_comperhensive" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="750"/>
    <metric>total_information_diffused</metric>
    <metric>days_worked</metric>
    <metric>total_gap_over_time</metric>
    <metric>disaster</metric>
    <metric>information_diffused_by_pms</metric>
    <metric>information_diffused_by_ims</metric>
    <metric>information_diffused_by_rs</metric>
    <enumeratedValueSet variable="deployment-frequency">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_activity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_number_of_PMs">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_deployment_length">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connect-to-colleagues?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average_number_of_remote_assessments">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-people-in-need">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_inter-org">
      <value value="45"/>
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forget-after-n-ticks">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epicenters">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS_publication_method">
      <value value="&quot;Accuracy-focused&quot;"/>
      <value value="&quot;Time-focused&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-map">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_activity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-location-shocks?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="goal">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-capacity-rss">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-disaster?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-scenario">
      <value value="&quot;Bangladesh-like&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PMs-per-tick-per-DoIA">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-PMs?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-names">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-length">
      <value value="730"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="share-international-local">
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="need-diffuse-coefficient">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shocks">
      <value value="6"/>
      <value value="12"/>
      <value value="18"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment_length">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-knowledge">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_deployment_length">
      <value value="147.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network-every-n-ticks">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-connections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-contacts">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IM-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stay-connected?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-accuracy-pms">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_intra-org">
      <value value="45"/>
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_IM_reporting_cycle">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-organisations">
      <value value="4"/>
      <value value="8"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Policy_experiment_effect_social_and_IM_sharing" repetitions="200" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="750"/>
    <metric>total_information_diffused</metric>
    <metric>days_worked</metric>
    <metric>total_gap_over_time</metric>
    <metric>disaster</metric>
    <metric>information_diffused_by_pms</metric>
    <metric>information_diffused_by_ims</metric>
    <metric>information_diffused_by_rs</metric>
    <enumeratedValueSet variable="deployment-frequency">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_activity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_number_of_PMs">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_deployment_length">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connect-to-colleagues?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average_number_of_remote_assessments">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-people-in-need">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_inter-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forget-after-n-ticks">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epicenters">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS_publication_method">
      <value value="&quot;Accuracy-focused&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-map">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_activity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-location-shocks?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="goal">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-capacity-rss">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-disaster?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-scenario">
      <value value="&quot;Bangladesh-like&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PMs-per-tick-per-DoIA">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-PMs?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-names">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-length">
      <value value="730"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="share-international-local">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="need-diffuse-coefficient">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shocks">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment_length">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-knowledge">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_deployment_length">
      <value value="147.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network-every-n-ticks">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-connections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-contacts">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IM-sharing">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stay-connected?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-accuracy-pms">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_intra-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-sharing">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_IM_reporting_cycle">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-organisations">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Policy_experiment_effect_hand_over_contacts_no_willingness" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="750"/>
    <metric>total_information_diffused</metric>
    <metric>days_worked</metric>
    <metric>total_gap_over_time</metric>
    <metric>disaster</metric>
    <metric>information_diffused_by_pms</metric>
    <metric>information_diffused_by_ims</metric>
    <metric>information_diffused_by_rs</metric>
    <enumeratedValueSet variable="deployment-frequency">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_activity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_number_of_PMs">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_deployment_length">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connect-to-colleagues?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average_number_of_remote_assessments">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-people-in-need">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_inter-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forget-after-n-ticks">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epicenters">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS_publication_method">
      <value value="&quot;Accuracy-focused&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-map">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_activity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-location-shocks?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="goal">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-capacity-rss">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-disaster?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-scenario">
      <value value="&quot;Bangladesh-like&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PMs-per-tick-per-DoIA">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-PMs?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-names">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-length">
      <value value="730"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="share-international-local">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="need-diffuse-coefficient">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shocks">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment_length">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-knowledge">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_deployment_length">
      <value value="147.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network-every-n-ticks">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-connections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-contacts">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IM-sharing">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stay-connected?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-accuracy-pms">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_intra-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-sharing">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_IM_reporting_cycle">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-organisations">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Policy_experiment_effect_hand_over_knowledge_no_willingness" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="750"/>
    <metric>total_information_diffused</metric>
    <metric>days_worked</metric>
    <metric>total_gap_over_time</metric>
    <metric>disaster</metric>
    <metric>information_diffused_by_pms</metric>
    <metric>information_diffused_by_ims</metric>
    <metric>information_diffused_by_rs</metric>
    <enumeratedValueSet variable="deployment-frequency">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_activity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_number_of_PMs">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_deployment_length">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connect-to-colleagues?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average_number_of_remote_assessments">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-people-in-need">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_inter-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forget-after-n-ticks">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epicenters">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS_publication_method">
      <value value="&quot;Accuracy-focused&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-map">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_activity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-location-shocks?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="goal">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-capacity-rss">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-disaster?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-scenario">
      <value value="&quot;Bangladesh-like&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PMs-per-tick-per-DoIA">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-PMs?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-names">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-length">
      <value value="730"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="share-international-local">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="need-diffuse-coefficient">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shocks">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment_length">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-knowledge">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_deployment_length">
      <value value="147.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network-every-n-ticks">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-connections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-contacts">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IM-sharing">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stay-connected?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-accuracy-pms">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_intra-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-sharing">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_IM_reporting_cycle">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-organisations">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Policy_experiment_comperhensive2" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="750"/>
    <metric>total_information_diffused</metric>
    <metric>days_worked</metric>
    <metric>total_gap_over_time</metric>
    <metric>disaster</metric>
    <metric>information_diffused_by_pms</metric>
    <metric>information_diffused_by_ims</metric>
    <metric>information_diffused_by_rs</metric>
    <enumeratedValueSet variable="deployment-frequency">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_activity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_number_of_PMs">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_deployment_length">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connect-to-colleagues?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average_number_of_remote_assessments">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-people-in-need">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Willingness_focus">
      <value value="&quot;Inward&quot;"/>
      <value value="&quot;Outward&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_inter-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forget-after-n-ticks">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epicenters">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS_publication_method">
      <value value="&quot;Accuracy-focused&quot;"/>
      <value value="&quot;Time-focused&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-map">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_activity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-location-shocks?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="goal">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-capacity-rss">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-disaster?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-scenario">
      <value value="&quot;Bangladesh-like&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PMs-per-tick-per-DoIA">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-PMs?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-names">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-length">
      <value value="730"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="share-international-local">
      <value value="0.6"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="need-diffuse-coefficient">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shocks">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment_length">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-knowledge">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_deployment_length">
      <value value="147.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network-every-n-ticks">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-connections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-contacts">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IM-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stay-connected?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-accuracy-pms">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_intra-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_IM_reporting_cycle">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-organisations">
      <value value="4"/>
      <value value="8"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Policy_experiment_effect_distributions_publication_method" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="750"/>
    <metric>total_information_diffused</metric>
    <metric>days_worked</metric>
    <metric>total_gap_over_time</metric>
    <metric>information_diffused_by_pms</metric>
    <metric>information_diffused_by_ims</metric>
    <metric>information_diffused_by_rs</metric>
    <enumeratedValueSet variable="deployment-frequency">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_activity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_number_of_PMs">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_deployment_length">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connect-to-colleagues?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average_number_of_remote_assessments">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-people-in-need">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_inter-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forget-after-n-ticks">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epicenters">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS_publication_method">
      <value value="&quot;Accuracy-focused&quot;"/>
      <value value="&quot;Time-focused&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-map">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_activity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-location-shocks?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="goal">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-capacity-rss">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-disaster?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-scenario">
      <value value="&quot;Bangladesh-like&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PMs-per-tick-per-DoIA">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-PMs?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-names">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-length">
      <value value="730"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="share-international-local">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="need-diffuse-coefficient">
      <value value="0.2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="shocks" first="2" step="2" last="40"/>
    <enumeratedValueSet variable="assessment_length">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-knowledge">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_deployment_length">
      <value value="147.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network-every-n-ticks">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-connections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-contacts">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IM-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stay-connected?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-accuracy-pms">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_intra-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_IM_reporting_cycle">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-organisations">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-accuracy">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Policy_experiment_effect_distributions_advanced" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="750"/>
    <metric>total_information_diffused</metric>
    <metric>days_worked</metric>
    <metric>total_gap_over_time</metric>
    <metric>information_diffused_by_pms</metric>
    <metric>information_diffused_by_ims</metric>
    <metric>information_diffused_by_rs</metric>
    <enumeratedValueSet variable="deployment-frequency">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_activity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_number_of_PMs">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_deployment_length">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connect-to-colleagues?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average_number_of_remote_assessments">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-people-in-need">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_inter-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forget-after-n-ticks">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epicenters">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS_publication_method">
      <value value="&quot;Accuracy-focused&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-map">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_activity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-location-shocks?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="goal">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-capacity-rss">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-disaster?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-scenario">
      <value value="&quot;Bangladesh-like&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PMs-per-tick-per-DoIA">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-PMs?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-names">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-length">
      <value value="730"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="share-international-local">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="need-diffuse-coefficient">
      <value value="0.2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="shocks" first="2" step="2" last="40"/>
    <enumeratedValueSet variable="assessment_length">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-knowledge">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_deployment_length">
      <value value="147.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network-every-n-ticks">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-connections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-contacts">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IM-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stay-connected?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-accuracy-pms">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_intra-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_IM_reporting_cycle">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-organisations">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distribution-accuracy">
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Policy_experiment_sensitivity_assessment_length" repetitions="50" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="750"/>
    <metric>total_information_diffused</metric>
    <metric>days_worked</metric>
    <metric>total_gap_over_time</metric>
    <metric>disaster</metric>
    <metric>information_diffused_by_pms</metric>
    <metric>information_diffused_by_ims</metric>
    <metric>information_diffused_by_rs</metric>
    <enumeratedValueSet variable="deployment-frequency">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_activity">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="min_number_of_PMs">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd_deployment_length">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="connect-to-colleagues?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average_number_of_remote_assessments">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="total-people-in-need">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_inter-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="forget-after-n-ticks">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="epicenters">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS_publication_method">
      <value value="&quot;Other&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-map">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="size_activity">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-location-shocks?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="goal">
      <value value="0.15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-capacity-rss">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-disaster?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RS-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-scenario">
      <value value="&quot;Bangladesh-like&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="PMs-per-tick-per-DoIA">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-PMs?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-names">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="disaster-length">
      <value value="730"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="share-international-local">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="need-diffuse-coefficient">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="shocks">
      <value value="12"/>
    </enumeratedValueSet>
    <steppedValueSet variable="assessment_length" first="4" step="2" last="12"/>
    <enumeratedValueSet variable="hand-over-knowledge">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean_deployment_length">
      <value value="147.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="extend-network-every-n-ticks">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="show-connections?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="hand-over-contacts">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="IM-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stay-connected?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sd-accuracy-pms">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="willingness_to_share_intra-org">
      <value value="45"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-sharing">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="length_IM_reporting_cycle">
      <value value="14"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-organisations">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

inactive-links
2.0
-0.2 0 0.0 1.0
0.0 1 4.0 4.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
