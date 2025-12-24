****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　伝票ＥＸＣＥＬ連携                *
*    モジュール名　　　　：　伝票ＥＸＣＥＬデータ件数確認      *
*    作成日／作成者　　　：　2016/09/16 INOUE                  *
*    処理概要　　　　　　：　パラメタより受け取ったデータ件数  *
*                            を表示する。　　　　　　　　　　  *
*    更新日／更新者　　　：　                                  *
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SDE0030I.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2016/09/16.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 表示ファイル >>--*
     SELECT   DSPFILE   ASSIGN         01-GS-DSPF
                        FORMAT         DSP-FMT
                        GROUP          DSP-GRP
                        PROCESSING     DSP-PRO
                        UNIT CONTROL   DSP-CON
                        FUNCTION       DSP-FNC
                        STATUS         DSP-ST.
*----<< 担当者マスタ >>--*
     SELECT   TANMS1    ASSIGN         DA-01-VI-TANMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TAN-F01 TAN-F02
                        STATUS         TAN-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FDE00301  OF        XMDLIB
              JOINING   DSP       PREFIX.
*----<< 担当者マスタ >>--*
 FD  TANMS1             LABEL RECORD   IS   STANDARD.
     COPY     TANMS1    OF        XFDLIB
              JOINING   TAN       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG        PIC  9(02)    VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  TAN-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).
 01  WK-SYSYMD.
     03  WK-SYSYY       PIC  9(04).
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSMM       PIC  Z9.
     03  FILLER         PIC  X(01)     VALUE "/".
     03  WK-SYSDD       PIC  Z9.
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  SYS-TIME2          PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME2.
     03  SYS-TIMEW      PIC  9(06).
     03  FILLER         PIC  9(02).
 01  G-TIME.
     03  G-TIME-HH                PIC  Z9.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  G-TIME-MM                PIC  Z9.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  G-TIME-SS                PIC  Z9.
*
*----<< ﾃﾞｨｽﾌﾟﾚｲ ｺﾝﾄﾛｰﾙ ｴﾘｱ >>-*
 01  GR-NO              PIC  9(02).
 01  WK-GRP             PIC  X(08).
 01  DSP-CNTL.
     03  DSP-ST         PIC  X(02).
     03  DSP-ST2        PIC  X(04).
     03  DSP-FMT        PIC  X(08).
     03  DSP-GRP        PIC  X(08).
     03  DSP-PRO        PIC  X(02).
     03  DSP-FNC        PIC  X(04).
     03  DSP-CON        PIC  X(06).
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ﾃ-ﾌﾞﾙ >>-*
 01  FNC-TABLE.
     03  ENT            PIC  X(04)     VALUE     "E000".
     03  PF04           PIC  X(04)     VALUE     "F004".
     03  PF05           PIC  X(04)     VALUE     "F005".
     03  PF06           PIC  X(04)     VALUE     "F006".
*
*----<< ﾌｱﾝｸｼﾖﾝ ｷｰ ｶﾞｲﾄﾞ >>-*
 01  GUIDE01       PIC  N(40)  VALUE   NC"_終　了".
 01  GUIDE02       PIC  N(40)  VALUE
*        NC"_取　消　_終　了　_項目戻り".
         NC"_終　了".
*
 01  MSG-AREA.
     03  MSG01               PIC  N(20)  VALUE
              NC"ＰＦキーが違います".
*    03  MSG02               PIC  N(20)  VALUE
*             NC"担当者コードを入力して下さい。".
     03  MSG03               PIC  N(20)  VALUE
              NC"各ファイル件数カウントエラー".
*
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS       3.
*
 01  SEC-AREA.
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-IN-JKUBUN         PIC   X(01).
 01  PARA-IN-EXCEL          PIC   X(01).
 01  PARA-IN-BUMON          PIC   X(04).
 01  PARA-IN-TANCD          PIC   X(02).
 01  PARA-IN-CNT1           PIC   9(07).
 01  PARA-IN-CNT2           PIC   9(07).
 01  PARA-IN-CNT3           PIC   9(07).
 01  PARA-IN-CNT4           PIC   9(07).
 01  PARA-IN-CNT5           PIC   9(07).
****************************************************************
 PROCEDURE              DIVISION USING PARA-IN-JKUBUN
                                       PARA-IN-EXCEL
                                       PARA-IN-BUMON
                                       PARA-IN-TANCD
                                       PARA-IN-CNT1
                                       PARA-IN-CNT2
                                       PARA-IN-CNT3
                                       PARA-IN-CNT4
                                       PARA-IN-CNT5.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 表示ファイル >>--*
 DSPFILE-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DSPFILE.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SDE0030I DSPFILE ERROR " DSP-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    TANMS1    DSPFILE.
     STOP     RUN.
*----<< 担当者マスタ >>--*
 TANMS1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TANMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SDE0030I TANMS1 ERROR " TAN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    TANMS1    DSPFILE.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE    "000-PROG-CNTL"      TO   S-NAME.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     GR-NO    =    99.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     MOVE    "100-INIT-RTN"       TO   S-NAME.
*T↓
*    DISPLAY NC"受取＿実行区分＝" PARA-IN-JKUBUN UPON CONS.
*    DISPLAY NC"受取＿ＥＸ種別＝" PARA-IN-EXCEL  UPON CONS.
*    DISPLAY NC"受取＿部門　　＝" PARA-IN-BUMON  UPON CONS.
*    DISPLAY NC"受取＿担当者　＝" PARA-IN-TANCD  UPON CONS.
*    DISPLAY NC"受取＿件数１　＝" PARA-IN-CNT1   UPON CONS.
*    DISPLAY NC"受取＿件数２　＝" PARA-IN-CNT2   UPON CONS.
*    DISPLAY NC"受取＿件数３　＝" PARA-IN-CNT3   UPON CONS.
*    DISPLAY NC"受取＿件数４　＝" PARA-IN-CNT4   UPON CONS.
*    DISPLAY NC"受取＿件数５　＝" PARA-IN-CNT5   UPON CONS.
*T↑
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
*      DISPLAY "LINK-YMD8 = " LINK-OUT-YMD8 UPON CONS
         MOVE LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     MOVE     SYS-YYW        TO   WK-SYSYY.
     MOVE     SYS-MMW        TO   WK-SYSMM.
     MOVE     SYS-DDW        TO   WK-SYSDD.
*
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SDE0030I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     TANMS1.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     MOVE     0              TO   GR-NO.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*----<< ｶﾞﾒﾝ ｸﾘｱ >>-*
     PERFORM  210-DSP-INIT   UNTIL     GR-NO    NOT  =    0.
*----<< ｶｸﾆﾝ ﾆｭｳﾘｮｸ >>-*
     PERFORM  230-INP-ENDCHK UNTIL     GR-NO    NOT  =    9.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
     CLOSE    DSPFILE.
     CLOSE    TANMS1.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SDE0030I END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      画面初期化                                  *
*--------------------------------------------------------------*
 210-DSP-INIT           SECTION.
     MOVE    "210-DSP-INIT"       TO   S-NAME.
     MOVE     SPACE               TO   DSP-FDE00301.
*
*    PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE    "SDE0030I"      TO   DSP-PGID.
     MOVE    "FDE00301"      TO   DSP-FORMID.
*モード表示
     IF       PARA-IN-JKUBUN        =   "1"
              MOVE NC"チェックのみ　"        TO   DSP-MODE
     END-IF.
     IF       PARA-IN-JKUBUN        =   "2"
              MOVE NC"チェック～変換"        TO   DSP-MODE
     END-IF.
*担当者名称取得
     MOVE      PARA-IN-BUMON       TO   TAN-F01 DSP-TANCD(1:4).
     MOVE      "-"                 TO           DSP-TANCD(5:1).
     MOVE      PARA-IN-TANCD       TO   TAN-F02 DSP-TANCD(6:2).
     READ      TANMS1
               INVALID  KEY
                        MOVE      SPACE     TO   DSP-TANNM
               NOT INVALID  KEY
                        MOVE      SPACE     TO   DSP-TANNM
                        MOVE      TAN-F03   TO   DSP-TANNM
     END-READ.
*EXCEL種別表示
     IF       PARA-IN-EXCEL         =   "1"
              MOVE    NC"伝票入力形式＿伝票番号指定"
                                               TO DSP-EXCEL
     END-IF.
     IF       PARA-IN-EXCEL         =   "2"
              MOVE    NC"伝票入力形式＿伝票番号自動採番"
                                               TO DSP-EXCEL
     END-IF.
     IF       PARA-IN-EXCEL         =   "3"
              MOVE    NC"量販店入力形式＿伝票自動採番"
                                               TO DSP-EXCEL
     END-IF.
*件数表示
* 取込件数
     MOVE     PARA-IN-CNT1                  TO   DSP-KENSU1.
* 売上伝票件数　　
     MOVE     PARA-IN-CNT2                  TO   DSP-KENSU2.
* 返品伝票件数
     MOVE     PARA-IN-CNT3                  TO   DSP-KENSU3.
* 値引伝票件数
     MOVE     PARA-IN-CNT4                  TO   DSP-KENSU4.
* エラー件数
     MOVE     PARA-IN-CNT5                  TO   DSP-KENSU5.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FDE00301"     TO   DSP-FMT.
     MOVE     "SCREEN"       TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*
     MOVE     9              TO   GR-NO.
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｶｸﾆﾝ ﾆｭｳﾘｮｸ                                 *
*--------------------------------------------------------------*
 230-INP-ENDCHK           SECTION.
     MOVE     "230-INP-ENDCHK"      TO   S-NAME.
     MOVE     "ENDCHK"         TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
         WHEN ENT
              CONTINUE
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 230-INP-ENDCHK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 900-DSP-READ           SECTION.
     MOVE     "900-DSP-READ"      TO   S-NAME.
     MOVE     "SCREEN"       TO   DSP-GRP.
     IF       ERR-FLG   =    0
              MOVE      SPACE               TO   DSP-MSGSPC
     ELSE
              MOVE      MSG-TBL (ERR-FLG)   TO   DSP-MSGSPC
     END-IF.
     IF       GR-NO     =    1
              MOVE      GUIDE01   TO   DSP-FNCSPC
     ELSE
              MOVE      GUIDE01   TO   DSP-FNCSPC
     END-IF.
     MOVE     WK-SYSYMD           TO   DSP-SDATE.
     ACCEPT   SYS-TIME2           FROM TIME.
*    MOVE     SYS-TIMEW           TO   STIME.
     MOVE     SYS-TIMEW(1:2)      TO   G-TIME-HH.
     MOVE     SYS-TIMEW(3:2)      TO   G-TIME-MM.
     MOVE     SYS-TIMEW(5:2)      TO   G-TIME-SS.
     MOVE     G-TIME              TO   DSP-STIME.
*
     PERFORM  900-DSP-WRITE.
*
     MOVE    "NE"            TO   DSP-PRO.
     MOVE     SPACE          TO   DSP-MSGSPC.
*
     MOVE     WK-GRP         TO   DSP-GRP.
     READ     DSPFILE.
     MOVE     SPACE          TO   DSP-PRO.
 900-DSP-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  WRITE                             *
*--------------------------------------------------------------*
 900-DSP-WRITE          SECTION.
     MOVE     "900-DSP-WRITE"     TO   S-NAME.
*****MOVE     SPACE          TO   DSP-PRO.
     WRITE    DSP-FDE00301.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
*CLR-HEAD-RTN           SECTION.
*    MOVE     "CLR-HEAD-RTN" TO   S-NAME.
*    MOVE     " "            TO   EDIT-CURSOR OF DSP-TANCD.
*    MOVE     "M"            TO   EDIT-OPTION OF DSP-TANCD.
*CLR-HEAD-EXIT.
*    EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＴＡＩＬ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-TAIL-RTN           SECTION.
     MOVE     "CLR-TAIL-RTN" TO   S-NAME.
     MOVE     " "            TO   EDIT-CURSOR OF DSP-ENDCHK.
     MOVE     "M"            TO   EDIT-OPTION OF DSP-ENDCHK.
 CLR-TAIL-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
