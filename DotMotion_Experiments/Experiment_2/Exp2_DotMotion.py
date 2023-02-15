# november 2021
# Internship project: Confidence bounds

# RANDOM DOT MOTION TASK (variant with 6 confidence options)
# (Does the majority of dots move left or right? How confident are you about your choice?)
# Participants complete multiple blocks of consecutive dot motion trials
# 3 training blocks with increasing complexity introduce participants to the task
# Manipulations: coherence (~ difficulty), accuracy/speed of the decision and accuracy/speed of the confidence ratings
# Resulting data: trial number, block, coherence, participant response, correct response, RT,
#                 confidence ratings, confidence RT, manipulation, label order
# subject numbers: minimum 1, maximum 40

# Importing modules
from psychopy import visual as vis
from psychopy import event, core, gui, data
import os
import random
import numpy as np
from time import sleep

pilot = 0  # Put to 0 for the real experiment

# Number of blocks/trials
nb_training_trials = 24  # Trials per training block; divisible by 3 (because 3 coherence manipulations) (24)
nb_main_trials = 60  # Trials per main block; divisible by 3 (because 3 coherence manipulations) (60)
nb_main_blocks = 12  # Without training blocks; divisible by 4 (because speed/accuracy manipulations) (12)

# Block 1
min_accuracy_1 = 0.85  # Minimum accuracy to necessary to continue to the next block (0.85)
p_coherence_1 = 0.5  # Percentage coherence (0.5)

# Block 2
min_accuracy_2 = 0.6  # (0.6)

# Block 2 (and every block afterwards)
p_coherence_a = 0.10  # Low coherence manipulation (0.10)
p_coherence_b = 0.20  # Medium coherence manipulation (0.20)
p_coherence_c = 0.40  # High coherence manipulation (0.40)

# Timing
time_cross = 1  # Time fixation cross (+ 0.5s when manipulations are shown) (1)
time_fb = 1  # Time feedback (1)


# Create a Data folder if it doesn't exist yet
my_directory = os.getcwd()
if not os.path.isdir('Exp2_DotMotion_Data'):
    os.mkdir('Exp2_DotMotion_Data')

# GUI
if pilot:
    sub = 0; age = 30; gender = 'Man'; handedness = 'Right'
    file_name = "Exp2_DotMotion_Data/DotsTask_sub%d" % sub
else:
    info = {"Subject number": 0, "gender": ['Woman', 'Man', 'X'], "age": 0, "handedness": ['Left', 'Right']}
    myDlg = gui.DlgFromDict(dictionary=info, title="DotsTask", show=True)
    sub = info['Subject number']; age = info['age']; gender = info['gender']; handedness = info['handedness']
    file_name = "Exp2_DotMotion_Data/DotsTask_sub%d" % sub
    if os.path.isfile(file_name):
        print('This subject number already exists!')
        core.quit()

# TrialHandler: make a data file
info = {"sub": sub, "age": age, "gender": gender, "handedness": handedness}
thisExp = data.ExperimentHandler(dataFileName=file_name, extraInfo=info)  # saving extra info

# Counterbalancing confidence labels
# 0 for high-low confidence; 1 for low-high confidence
if sub % 2 == 0:
    confidence_labels = 0
else:
    confidence_labels = 1

# Counterbalancing manipulation order
# Number from 1 to 4; refers to the order of accurate/fast manipulations (Latin square counterbalanced)
if 0 < sub <= 10:
    manipulation_order = 1
elif 10 < sub <= 20:
    manipulation_order = 2
elif 20 < sub <= 30:
    manipulation_order = 3
elif 30 < sub <= 40:
    manipulation_order = 4
else:
    print("incorrect subject number")
    core.quit()

# Clock
clock = core.Clock()

# Visual features
win = vis.Window(size=[1920, 1080], color='black', allowGUI=False, units='norm', fullscr=False)
win.mouseVisible = False
fix = vis.TextStim(win, text="+", color='white')
good = vis.TextStim(win, text="Correct!", color='green')
bad = vis.TextStim(win, text="Wrong...", color='red')

# Introduction images
Introduction = vis.TextStim(win, text="Welcome to this experiment!\n\nOn each turn, you will see a series of moving "
                                      "dots in the center of the screen.\nSome of those dots will consistently move in "
                                      "the same direction: either to the left or to the right.\nYour job is to "
                                      "determine in which direction most of the dots are moving."
                                      "\n\n\n\n\n\n\n\n\n\n\n\n\n"
                                      "In this example, most of the dots move to the left.", pos=(0, 0.15), height=.05)
Intro_block1 = vis.ImageStim(win,
                             image=my_directory+'\\Exp2_DotMotion_Instructions\\Exp2_DotMotion_Intro_Block1.JPG',
                             pos=(0, 0.1))
Intro_block2 = vis.ImageStim(win,
                             image=my_directory+'\\Exp2_DotMotion_Instructions\\Exp2_DotMotion_Intro_Block2.JPG',
                             pos=(0, 0.1))
Intro_block3a = vis.ImageStim(win,
                              image=my_directory+'\\Exp2_DotMotion_Instructions\\Exp2_DotMotion_Intro_Block3a.JPG',
                              pos=(0, 0.1))
Intro_block3b = vis.ImageStim(win,
                              image=my_directory+'\\Exp2_DotMotion_Instructions\\Exp2_DotMotion_Intro_Block3b.JPG',
                              pos=(0, 0.1))
Intro_block4 = vis.ImageStim(win,
                             image=my_directory+'\\Exp2_DotMotion_Instructions\\Exp2_DotMotion_Intro_Block4.JPG',
                             pos=(0, 0.1))

# Manipulation images
FastFast = vis.ImageStim(win, image=my_directory+'\\Exp2_DotMotion_Instructions\\Exp2_DotMotion_FastFast.JPG',
                         pos=(0, 0))
AccAcc = vis.ImageStim(win, image=my_directory+'\\Exp2_DotMotion_Instructions\\Exp2_DotMotion_AccAcc.JPG',
                       pos=(0, 0))
FastAcc = vis.ImageStim(win, image=my_directory+'\\Exp2_DotMotion_Instructions\\Exp2_DotMotion_FastAcc.JPG',
                        pos=(0, 0))
AccFast = vis.ImageStim(win, image=my_directory+'\\Exp2_DotMotion_Instructions\\Exp2_DotMotion_AccFast.JPG',
                        pos=(0, 0))

# Manipulations labels
decision_Fast = vis.TextStim(win, text='Make FAST decisions', pos=(0, 0.3), height=.07)
decision_Accurate = vis.TextStim(win, text='Make ACCURATE decisions', pos=(0, 0.3), height=.07)
confidence_Fast = vis.TextStim(win, text='Give FAST confidence ratings', pos=(0, -0.3), height=.07)
confidence_Accurate = vis.TextStim(win, text='Think CAREFULLY about your confidence ratings', pos=(0, -0.3), height=.07)

# Warning label
warning = vis.TextStim(win, text='Too slow... Please respond faster', pos=(0, 0), height=.07, color='red')

# Confidence labels, counterbalance the order between participants
if confidence_labels == 0:
    cj_labels = vis.ImageStim(win,
                              image=my_directory+'\\Exp2_DotMotion_Instructions\\Exp2_DotMotion_ConfidenceLabels_b.JPG',
                              pos=(0, 0))
elif confidence_labels == 1:
    cj_labels = vis.ImageStim(win,
                              image=my_directory+'\\Exp2_DotMotion_Instructions\\Exp2_DotMotion_ConfidenceLabels_a.JPG',
                              pos=(0, 0))

# Space to continue
space = vis.TextStim(win, text='Press space to continue', pos=(0, -0.85), height=0.05)

# Ending screen
end = vis.TextStim(win, text='The end! Thank you for participating.\n\nPlease wait in silence until everyone is ready.',
                   pos=(0, 0), height=.07)

# Define keys
choice_keys = ['c', 'n', 'escape']  # left, right, escape
cj_keys = ['1', '2', '3', '8', '9', '0', 'escape']

# Creating DotMotion stimulus
DotMotion = vis.DotStim(win, units='pix', nDots=120, fieldShape='circle', dotSize=4, color='white', speed=1,
                        signalDots='same', noiseDots='direction', dotLife=5)

# Initialize variables
ACC = 0
n_manipulations = 0

# Order of manipulations: Latin square counterbalanced across participants
if manipulation_order == 1:
    manipulations = ['FastFast', 'AccFast', 'AccAcc', 'FastAcc', 'FastFast', 'AccFast', 'AccAcc', 'FastAcc', 'FastFast',
                     'AccFast', 'AccAcc', 'FastAcc']
elif manipulation_order == 2:
    manipulations = ['FastAcc', 'FastFast', 'AccFast', 'AccAcc', 'FastAcc', 'FastFast', 'AccFast', 'AccAcc', 'FastAcc',
                     'FastFast', 'AccFast', 'AccAcc']
elif manipulation_order == 3:
    manipulations = ['AccAcc', 'FastAcc', 'FastFast', 'AccFast', 'AccAcc', 'FastAcc', 'FastFast', 'AccFast', 'AccAcc',
                     'FastAcc', 'FastFast', 'AccFast']
elif manipulation_order == 4:
    manipulations = ['AccFast', 'AccAcc', 'FastAcc', 'FastFast', 'AccFast', 'AccAcc', 'FastAcc', 'FastFast', 'AccFast',
                     'AccAcc', 'FastAcc', 'FastFast']

# Practice trials 1 = 1
# Practice trials 2 = 2 (lower coherence)
# Practice trials 3 = 3 (adding confidence)
# Main trials = 4
blocks = nb_main_blocks + 4
for block in range(1,blocks):

    accuracy = 0

    if block == 1:
        # Drawing introduction screen + introduction dot motion trial
        resp = []
        DotMotion.coherence = 0.8
        DotMotion.dir = 180
        DotMotion.fieldSize = 300
        DotMotion.dotLife = 8
        while len(resp) == 0:
            DotMotion.draw()
            Introduction.draw()
            space.draw()
            win.flip()
            resp = event.getKeys(keyList='space')
        DotMotion.fieldSize = 500
        DotMotion.dotLife = 5

        # Settings block 1
        min_accuracy = min_accuracy_1
        p_coherence = p_coherence_1
        n_trials = nb_training_trials

        # Introduction block 1
        Intro_block1.draw(); space.draw(); win.flip()
        event.waitKeys(keyList='space')

    if block == 2:
        # Settings block 2
        min_accuracy = min_accuracy_2
        n_trials = nb_training_trials

        # Introduction block 2
        Intro_block2.draw(); space.draw(); win.flip()
        event.waitKeys(keyList='space')

    if block == 3:
        # Settings block 3
        accuracy = -1
        min_accuracy = 0
        n_trials = nb_training_trials

        # Introduction block 3
        if confidence_labels == 0:
            Intro_block3b.draw()
        else:
            Intro_block3a.draw()
        space.draw(); win.flip()
        event.waitKeys(keyList='space')

    if block == 4:
        # Introduction block 4
        Intro_block4.draw(); space.draw(); win.flip()
        event.waitKeys(keyList='space')

        # Manipulations shown -> longer fixation cross time
        time_cross = time_cross + 0.5

    # Randomise manipulations in block 4+
    if 4 <= block <= 3 + nb_main_blocks:
        accuracy = -1
        min_accuracy = 0
        n_trials = nb_main_trials

        # Drawing the manipulation instruction
        if manipulations[n_manipulations] == 'FastFast':
            decision_instruction = decision_Fast
            confidence_instruction = confidence_Fast
            FastFast.draw(); space.draw(); win.flip()
            event.waitKeys(keyList='space')
        if manipulations[n_manipulations] == 'AccAcc':
            decision_instruction = decision_Accurate
            confidence_instruction = confidence_Accurate
            AccAcc.draw(); space.draw(); win.flip()
            event.waitKeys(keyList='space')
        if manipulations[n_manipulations] == 'FastAcc':
            decision_instruction = decision_Fast
            confidence_instruction = confidence_Accurate
            FastAcc.draw(); space.draw(); win.flip()
            event.waitKeys(keyList='space')
        if manipulations[n_manipulations] == 'AccFast':
            decision_instruction = decision_Accurate
            confidence_instruction = confidence_Fast
            AccFast.draw(); space.draw(); win.flip()
            event.waitKeys(keyList='space')

    # Randomise coherence levels, counterbalanced within the participant
    if block > 1:
        coherence_list = np.repeat([p_coherence_a, p_coherence_b, p_coherence_c], n_trials/3)
        random.shuffle(coherence_list)

    repetition = 0

    # Single block loop
    while accuracy < min_accuracy:
        accuracy = 0
        RT_mean = 0
        RTconf_mean = 0
        repetition = repetition + 1
        n_slowtrials = 0

        # Randomise left vs. right dot movement
        condition = np.repeat(range(2),n_trials/2)  # 0 = left correct; 1 = right correct
        random.shuffle(condition)

        # Start trial loop
        for trial in range(n_trials):
            SlowTrial = 0
            if block > 1:
                p_coherence = coherence_list[trial]
            if condition[trial] == 0:
                correct = 'left'
                direction = 180
            if condition[trial] == 1:
                correct = 'right'
                direction = 0

            # Fixation cross
            fix.draw()
            if block > 3:
                decision_instruction.draw()
                confidence_instruction.draw()
            win.flip()
            sleep(time_cross)
            clock.reset()

            # Dot motion settings
            resp = []; conf_press = []; event.clearEvents(); RT = 0; RTconf = 0
            DotMotion.coherence = p_coherence
            DotMotion.dir = direction

            # Random dot motion task
            while RT < 5:
                DotMotion.draw()
                win.flip()
                resp = event.getKeys(keyList=choice_keys)
                RT = clock.getTime()
                if len(resp) > 0:
                    break
            if RT >= 5:
                resp = 'x'
                SlowTrial = 1
                n_slowtrials = n_slowtrials + 1
                warning.draw(); space.draw()
                win.flip()
                event.waitKeys(keyList='space')
            win.flip()

            if resp == ['escape']:
                print('Participant pressed escape')
                win.close()
                core.quit()

            # Evaluating response
            if correct == 'left' and resp[0] == choice_keys[0]:
                ACC = 1
            elif correct == 'right' and resp[0] == choice_keys[0]:
                ACC = 0
            elif correct == 'left' and resp[0] == choice_keys[1]:
                ACC = 0
            elif correct == 'right' and resp[0] == choice_keys[1]:
                ACC = 1
            else:
                ACC = -99

            if block == 1 or block == 2:
                if ACC == 1:
                    good.draw()
                elif ACC == 0:
                    bad.draw()
                win.flip()
                sleep(time_fb)

            # Ask for confidence about the choice after from the third block on
            if block > 2 and resp != 'x':
                clock.reset()
                event.clearEvents()
                while RTconf < 5:
                    cj_labels.draw()
                    win.flip()
                    conf_press = event.getKeys(keyList=cj_keys)
                    RTconf = clock.getTime()
                    if len(conf_press) > 0:
                        break
                if RTconf >= 5:
                    conf_press = 'x'
                    SlowTrial = 1
                    n_slowtrials = n_slowtrials + 1
                    warning.draw(); space.draw()
                    win.flip()
                    event.waitKeys(keyList='space')
                win.flip()

                # Convert conf_press into numeric value from 1 (sure error) to 6 (sure correct)
                if SlowTrial == 0:
                    for temp in range(0,6):
                        if conf_press[0] == cj_keys[temp]:
                            cj = temp + 1
                    # Reverse order for half
                    if confidence_labels == 0:
                        cj = 7 - cj
                else:
                    conf_press = 'none'
                    cj = -99
                    RTconf = -99

            else:
                conf_press = 'none'
                cj = -99
                RTconf = -99

            # Store data of current trial
            thisExp.addData("withinblocktrial", trial)
            thisExp.addData("block", block)
            thisExp.addData("block_repetition", repetition)
            thisExp.addData("rt", RT)
            thisExp.addData("resp", resp)
            thisExp.addData("cor", ACC)
            thisExp.addData("cresp", correct)
            thisExp.addData("conf_press", conf_press)
            thisExp.addData("cj", cj)
            thisExp.addData("rtconf", RTconf)
            thisExp.addData("coherence", p_coherence)
            thisExp.addData("slow_trial", SlowTrial)
            if block > 3:
                thisExp.addData("manipulation", manipulations[n_manipulations])
            else:
                thisExp.addData("manipulation", 'none')

            # Pressing escape
            if conf_press == ['escape']:
                print('Participant pressed escape')
                win.close()
                core.quit()

            # Proceed to next trial
            thisExp.nextEntry()

            # Add data to variables
            if SlowTrial == 0:
                accuracy = accuracy + ACC
                RT_mean = RT_mean + RT
                RTconf_mean = RTconf_mean + RTconf

        # Mean of variables of interest
        accuracy = accuracy/n_trials
        p_accuracy = 100*accuracy
        if (n_trials - n_slowtrials) != 0:
            RT_mean = RT_mean/(n_trials - n_slowtrials)
            RTconf_mean = RTconf_mean/(n_trials - n_slowtrials)

        # Show accuracy, mean RT and mean confidence RT
        if block < 3:
            pause = vis.TextStim(win,text='Time for a break\n\n\nResults of the last block:\n\nAccuracy: ' +
                                          str(round(p_accuracy, 2)) +
                                          '%\nAverage decision reaction time: ' + str(round(RT_mean, 2)) +
                                          ' seconds', pos=(0,0), height=.05)
        elif block == 3:
            pause = vis.TextStim(win,text='Time for a break\n\n\nResults of the last block:\n\nAccuracy: ' +
                                          str(round(p_accuracy, 2)) +
                                          '%\nAverage decision reaction time: ' + str(round(RT_mean, 2)) +
                                          ' seconds\nAverage confidence reaction time: ' + str(round(RTconf_mean, 2)) +
                                          ' seconds', pos=(0,0), height=.05)
        else:
            pause = vis.TextStim(win,text='Time for a break\n\n\nResults of the last block:\n\nAccuracy: ' +
                                          str(round(p_accuracy, 2)) +
                                          '%\nAverage decision reaction time: ' + str(round(RT_mean, 2)) +
                                          ' seconds\nAverage confidence reaction time: ' + str(round(RTconf_mean, 2)) +
                                          ' seconds\n\n\n' + str(nb_main_blocks-(n_manipulations+1)) +
                                          ' more block(s) to go\nRemember to try and use all confidence options '
                                          'equally', pos=(0, 0), height=.05)

        pause.draw(); space.draw(); win.flip()
        event.waitKeys(keyList='space')

    # Move on to the next manipulation
    if block > 3:
        n_manipulations = n_manipulations + 1

end.draw();win.flip()
event.waitKeys(keyList='space')

# End of the experiment
win.close()
core.quit()
