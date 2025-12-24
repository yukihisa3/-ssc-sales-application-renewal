****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷　　　                        *
*    サブシステム　　　　：　コーナン　ＥＤＩ　　　　　　　　　*
*    モジュール名　　　　：　手書ＰＤラベル発行データ　　      *
*    　　　　　　　　　　　　作成指示　　　　　　　　　　      *
*    作成日／作成者　　　：　2021/09/13                        *
*    処理概要　　　　　　：　作成条件（範囲）の指定を行い、    *
*                            パラメタに引き渡す。　　　　　    *
*    更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：　                                  *
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY1326I.
*                  流用:SSY1226I
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/09/13.
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
*----<< 手書ＰＤラベルファイル >>--*
     SELECT   KTBULKF   ASSIGN         DA-01-VI-KTBULKL2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  BUL-F001  BUL-F002
                                       BUL-F003  BUL-F004
                                       BUL-FA01  BUL-FA02
                                       BUL-FA03  BUL-FA06
                                       BUL-FA04  BUL-FA05
                                       BUL-FB06
                        STATUS         KTBULKF-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         HTOKMS-ST.
*----<< 倉庫マスタ >>--*
     SELECT   ZSOKMS1   ASSIGN         DA-01-VI-ZSOKMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SOK-F01
                        STATUS         ZSOKMS1-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FSY13261  OF        XMDLIB.
*----<< 手書ラベルファイル >>--*
 FD  KTBULKF            LABEL     RECORD   IS   STANDARD.
     COPY     KTBULKF   OF        XFDLIB
              JOINING   BUL       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 倉庫マスタ >>--*
 FD  ZSOKMS1            LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS1   OF        XFDLIB
              JOINING   SOK       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG        PIC  9(02)    VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  KTBULKF-ST        PIC  X(02).
 01  HTOKMS-ST         PIC  X(02).
 01  ZSOKMS1-ST        PIC  X(02).
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
 01  WK-NOUYMD.
     03  WK-NOUYY       PIC  9(04)     VALUE  ZERO.
     03  WK-NOUMM       PIC  9(02)     VALUE  ZERO.
     03  WK-NOUDD       PIC  9(02)     VALUE  ZERO.
 01  WK-NOUYMD-R        REDEFINES      WK-NOUYMD.
     03  WK-NOUHIN      PIC  9(08).
 01  WK-NOUYMD-RR       REDEFINES      WK-NOUYMD.
     03  WK-NOUY1       PIC  9(02).
     03  WK-NOUYMD2     PIC  9(06).
 01  WK-HACYMD.
     03  WK-HACYY       PIC  9(04)     VALUE  ZERO.
     03  WK-HACMM       PIC  9(02)     VALUE  ZERO.
     03  WK-HACDD       PIC  9(02)     VALUE  ZERO.
 01  WK-HACYMD-R        REDEFINES      WK-HACYMD.
     03  WK-HACYU       PIC  9(08).
 01  WK-HACYMD-RR       REDEFINES      WK-HACYMD.
     03  WK-HACY1       PIC  9(02).
     03  WK-HACYMD2     PIC  9(06).
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
         NC"_取　消　_終　了　_項目戻り".
*
 01  MSG-AREA.
     03  MSG01               PIC  N(20)  VALUE
              NC"ＰＦキーが違います".
     03  MSG02               PIC  N(20)  VALUE
              NC"バッチ_を入力して下さい。".
     03  MSG03               PIC  N(20)  VALUE
              NC"バッチ_に誤りがあります。".
     03  MSG04               PIC  N(20)  VALUE
              NC"取引先マスタ未登録です。".
     03  MSG05               PIC  N(20)  VALUE
              NC"指定されたバッチが存在しません。".
     03  MSG06               PIC  N(20)  VALUE
              NC"　　　　　　　　　".
     03  MSG07               PIC  N(20)  VALUE
              NC"開始が終了を超えています。".
     03  MSG08               PIC  N(20)  VALUE
              NC"倉庫マスタ未登録です。".
     03  MSG09               PIC  N(20)  VALUE
              NC"正しい日付を入力してください。".
     03  MSG10               PIC  N(20)  VALUE
              NC"正しい日付を入力してください。".
     03  MSG11               PIC  N(20)  VALUE
              NC"対象データが存在しません。".
*
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS       11.
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
*IN
 01  PARA-IN-BUMON              PIC   X(04).
 01  PARA-IN-TANTOU             PIC   X(02).
 01  PARA-IN-SOKO               PIC   X(02).
 01  PARA-IN-DSOKO              PIC   X(02).
*OUT
 01  PARA-OUT-JDATE             PIC   9(08).
 01  PARA-OUT-JTIME             PIC   9(04).
 01  PARA-OUT-TORICD            PIC   9(08).
 01  PARA-OUT-SOKO              PIC   X(02).
 01  PARA-OUT-STENPO            PIC   9(05).
 01  PARA-OUT-ETENPO            PIC   9(05).
 01  PARA-OUT-SROUTE            PIC   9(02).
 01  PARA-OUT-EROUTE            PIC   9(02).
 01  PARA-OUT-SBUMON            PIC   9(02).
 01  PARA-OUT-EBUMON            PIC   9(02).
 01  PARA-OUT-HDATE             PIC   9(08).
 01  PARA-OUT-NDATE             PIC   9(08).
*01  PARA-OUT-STANA             PIC   X(06).
*01  PARA-OUT-ETANA             PIC   X(06).
*
****************************************************************
 PROCEDURE              DIVISION USING
                                  PARA-IN-BUMON
                                  PARA-IN-TANTOU
                                  PARA-IN-SOKO
                                  PARA-IN-DSOKO
                                  PARA-OUT-JDATE
                                  PARA-OUT-JTIME
                                  PARA-OUT-TORICD
                                  PARA-OUT-SOKO
                                  PARA-OUT-STENPO
                                  PARA-OUT-ETENPO
                                  PARA-OUT-SROUTE
                                  PARA-OUT-EROUTE
                                  PARA-OUT-SBUMON
                                  PARA-OUT-EBUMON
                                  PARA-OUT-HDATE
                                  PARA-OUT-NDATE.
*                                 PARA-OUT-STANA
*                                 PARA-OUT-ETANA.
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
     DISPLAY  "### SSY1326I DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
*    CLOSE    KTBULKF   HTOKMS    ZSOKMS1    DSPFILE.
     STOP     RUN.
*----<< 手書ラベルファイル >>--*
 KTBULKF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KTBULKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY1326I KTBULKL2 ERROR " KTBULKF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
*    CLOSE    KTBULKF   ZSOKMS1    DSPFILE.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY1326I HTOKMS ERROR " HTOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
*    CLOSE    KTBULKF   HTOKMS    ZSOKMS1    DSPFILE.
     STOP     RUN.
*----<< 倉庫マスタ >>--*
 ZSOKMS1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZSOKMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SSY1326I ZSOKMS1 ERROR " ZSOKMS1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
*    CLOSE    KTBULKF   ZSOKMS1    DSPFILE.
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
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
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
     DISPLAY  "*** SSY1326I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     KTBULKF.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     ZSOKMS1.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     MOVE     0              TO   GR-NO.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE    "200-MAIN-RTN"       TO   S-NAME.
*----<< ﾊﾝｲ ｼﾃｲ ｶﾞﾒﾝ ｸﾘｱ >>-*
     PERFORM  210-DSP-INIT   UNTIL     GR-NO    NOT  =    0.
*----<< ﾊﾞｯﾁNO. ﾆｭｳﾘｮｸ >>-*
     PERFORM  220-INP-GRP01  UNTIL     GR-NO    NOT  =    1.
*----<< ﾊﾝｲ ｼﾃｲ ﾆｭｳﾘｮｸ1 >>-*
     PERFORM  220-INP-GRP02  UNTIL     GR-NO    NOT  =    2.
*----<< ﾊﾝｲ ｼﾃｲ ﾆｭｳﾘｮｸ2 >>-*
     PERFORM  220-INP-GRP03  UNTIL     GR-NO    NOT  =    3.
*----<< ｶｸﾆﾝ ﾆｭｳﾘｮｸ >>-*
     PERFORM  230-INP-KKNN   UNTIL     GR-NO    NOT  =    9.
*----<< ﾊﾟﾗﾒﾀ ｼｭﾂﾘｮｸ >>-*
     PERFORM  240-PARA-OUT-SEC UNTIL   GR-NO    NOT  =    10.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
     CLOSE    DSPFILE.
     CLOSE    KTBULKF.
     CLOSE    HTOKMS.
     CLOSE    ZSOKMS1.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY1326I END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾃﾞｨｽﾌﾟﾚｰ  ｼｮｷ ﾋｮｳｼﾞ                         *
*--------------------------------------------------------------*
 210-DSP-INIT           SECTION.
     MOVE    "210-DSP-INIT"       TO   S-NAME.
     MOVE     SPACE          TO   FSY13261.
*
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-BODY1-RTN.
     PERFORM  CLR-BODY2-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE    "SSY1326I"      TO   PGID.
     MOVE    "FSY13261"      TO   FORM.
     MOVE     WK-SYSYMD      TO   SYSYMD.
*
*倉庫コードパラメタより倉庫コード入力チェック
     DISPLAY "PARA-IN-SOKO  = " PARA-IN-SOKO  UPON CONS.
     DISPLAY "PARA-IN-DSOKO = " PARA-IN-DSOKO UPON CONS.
     IF       PARA-IN-DSOKO  =  "01"  OR  "88"
              MOVE    " "    TO   EDIT-STATUS OF SOKO
     ELSE
              MOVE      PARA-IN-SOKO TO   SOK-F01 SOKO
              READ      ZSOKMS1
                  INVALID  KEY
                        MOVE      ALL NC"＊" TO   SOKONM
              NOT INVALID  KEY
                        MOVE      SOK-F02    TO   SOKONM
              END-READ
              MOVE    "X"    TO   EDIT-STATUS OF SOKO
     END-IF.
*出力パターンセット
*    MOVE     1              TO   PATAN.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FSY13261"     TO   DSP-FMT.
     MOVE     "SCREEN"       TO   DSP-GRP.
     PERFORM  900-DSP-WRITE.
*
     MOVE     1              TO   GR-NO.
 210-DSP-INIT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾞｯﾁNO.  ﾆｭｳﾘｮｸ                             *
*--------------------------------------------------------------*
 220-INP-GRP01          SECTION.
     MOVE     "220-INP-GRP01"     TO   S-NAME.
     MOVE     "HEAD01"       TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE      4010      TO   PROGRAM-STATUS
         WHEN ENT
              PERFORM   CLR-HEAD-RTN
              PERFORM   220-GRP01-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      2    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP01-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾞｯﾁNO. ﾁｪｯｸ                                *
*--------------------------------------------------------------*
 220-GRP01-CHECK-SEC    SECTION.
     MOVE     "220-CRP01-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*
     IF     ( JDATE     =    ZERO ) AND
            ( JTIME     =    ZERO ) AND
            ( TORICD    =    ZERO )
              MOVE      2         TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF JDATE
              MOVE     "R"        TO   EDIT-OPTION OF JDATE
              MOVE     "R"        TO   EDIT-OPTION OF JTIME
              MOVE     "R"        TO   EDIT-OPTION OF TORICD
              GO   TO   220-GRP01-CHECK-EXIT
     END-IF.
*    受信日付チェック
     IF  JDATE     NOT =     ZERO
         MOVE     "2"        TO        LINK-IN-KBN
         MOVE      JDATE     TO        LINK-IN-YMD8
         CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD8
         IF   LINK-OUT-RET   NOT =     ZERO
              MOVE      10        TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF JDATE
              MOVE     "R"        TO   EDIT-OPTION OF JDATE
              GO   TO   220-GRP01-CHECK-EXIT
         END-IF
     END-IF.
*    受信時間
     IF  JTIME     NOT NUMERIC
         MOVE      ZERO      TO   JTIME
     END-IF.
*    取引先チェック
     IF  TORICD    NOT =     ZERO
         MOVE      SPACE     TO   TOK-REC
         INITIALIZE               TOK-REC
         MOVE      TORICD    TO   TOK-F01
         READ      HTOKMS
             INVALID
                   MOVE      4    TO   ERR-FLG
                   MOVE     "C"   TO   EDIT-CURSOR OF TORICD
                   MOVE     "R"   TO   EDIT-OPTION OF TORICD
         END-READ
     END-IF.
*    手書ラベルファイル存在チェック
     IF  ERR-FLG        =    ZERO
         MOVE      SPACE          TO   BUL-REC
         INITIALIZE                    BUL-REC
         MOVE      JDATE          TO   BUL-F001
         MOVE      JTIME          TO   BUL-F002
         MOVE      TORICD         TO   BUL-F003
         MOVE      SPACE          TO   BUL-F004
         MOVE      ZERO           TO   BUL-FA01
         MOVE      ZERO           TO   BUL-FA02
         MOVE      ZERO           TO   BUL-FA03
         MOVE      ZERO           TO   BUL-FA06
         MOVE      ZERO           TO   BUL-FA04
         MOVE      ZERO           TO   BUL-FA05
         MOVE      ZERO           TO   BUL-FB06
         START     KTBULKF   KEY  >=   BUL-F001  BUL-F002
                                       BUL-F003  BUL-F004
                                       BUL-FA01  BUL-FA02
                                       BUL-FA03  BUL-FA06
                                       BUL-FA04  BUL-FA05
                                       BUL-FB06
              INVALID   KEY
                   MOVE      5    TO   ERR-FLG
                   MOVE     "C"   TO   EDIT-CURSOR OF JDATE
                   MOVE     "R"   TO   EDIT-OPTION OF JDATE
                   MOVE     "R"   TO   EDIT-OPTION OF JTIME
                   MOVE     "R"   TO   EDIT-OPTION OF TORICD
****************DISPLAY "AAA" UPON CONS
                   GO   TO   220-GRP01-CHECK-EXIT
              NOT INVALID
                   READ      KTBULKF
                     AT END
                        MOVE      5    TO   ERR-FLG
                        MOVE     "C"   TO   EDIT-CURSOR OF JDATE
                        MOVE     "R"   TO   EDIT-OPTION OF JDATE
                        MOVE     "R"   TO   EDIT-OPTION OF JTIME
                        MOVE     "R"   TO   EDIT-OPTION OF TORICD
********************DISPLAY "BBB" UPON CONS
                        GO   TO   220-GRP01-CHECK-EXIT
                     NOT AT END
************DISPLAY "BUL-F001= " BUL-F001 UPON CONS
*           DISPLAY "BUL-F002= " BUL-F002 UPON CONS
*           DISPLAY "BUL-F003= " BUL-F003 UPON CONS
*           DISPLAY "JDATE   = " JDATE    UPON CONS
*           DISPLAY "JTIME   = " JTIME    UPON CONS
************DISPLAY "TORICD  = " TORICD   UPON CONS
                        IF ( JDATE     =    BUL-F001 ) AND
                           ( JTIME     =    BUL-F002 ) AND
                           ( TORICD    =    BUL-F003 )
                             CONTINUE
                        ELSE
****************DISPLAY "CCC" UPON CONS
                             MOVE      5    TO   ERR-FLG
                             MOVE "C"  TO   EDIT-CURSOR OF JDATE
                             MOVE "R"  TO   EDIT-OPTION OF JDATE
                             MOVE "R"  TO   EDIT-OPTION OF JTIME
                             MOVE "R"  TO   EDIT-OPTION OF TORICD
                             GO   TO   220-GRP01-CHECK-EXIT
                        END-IF
                   END-READ
         END-START
     END-IF.
*
 220-GRP01-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾝｲ      ﾆｭｳﾘｮｸ1                            *
*--------------------------------------------------------------*
 220-INP-GRP02          SECTION.
     MOVE     "220-INP-GRP02"     TO   S-NAME.
*
     MOVE     "BODY01"            TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE      4010      TO   PROGRAM-STATUS
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF06
              MOVE      1         TO   GR-NO
         WHEN ENT
              PERFORM   CLR-BODY1-RTN
              PERFORM   220-GRP02-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      3    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE      1                   TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP02-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3                                                  *
*--------------------------------------------------------------*
 220-GRP02-CHECK-SEC    SECTION.
     MOVE     "220-CRP02-CHECK-SEC"    TO   S-NAME.
*
*    MOVE     "SCREEN"       TO   DSP-GRP.
*    PERFORM  900-DSP-WRITE.
*
     MOVE     ZERO      TO        ERR-FLG.
*    対象倉庫コードチェック
*****IF     ( SOKO      IS   NUMERIC   )   AND
*****       ( SOKO      NOT =     ZERO )
     IF     ( SOKO      NOT =  SPACE   )
              MOVE      SOKO      TO   SOK-F01
              READ      ZSOKMS1
                  INVALID  KEY
                        MOVE      SPACE     TO   SOKONM
                        MOVE      8    TO   ERR-FLG
                        MOVE     "C"   TO   EDIT-CURSOR OF SOKO
                        MOVE     "R"   TO   EDIT-OPTION OF SOKO
                  NOT INVALID  KEY
                        MOVE      SOK-F02   TO   SOKONM
*                       PERFORM   220-SONZAI-CHECK
              END-READ
     ELSE
              MOVE      SPACE     TO   SOKONM
     END-IF.
*    店舗コード
     IF       STEN     NOT NUMERIC
              MOVE     00000 TO   STEN
     END-IF.
     IF       ETEN     NOT NUMERIC
              MOVE     99999 TO   ETEN
     END-IF.
     IF     ( STEN      IS NUMERIC     AND
              ETEN      IS NUMERIC     AND
              STEN      >    ETEN   )
              IF   ERR-FLG   =    ZERO
                   MOVE      7    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF STEN
              MOVE     "R"   TO   EDIT-OPTION OF STEN
              MOVE     "R"   TO   EDIT-OPTION OF ETEN
     END-IF.
*
 220-GRP02-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾝｲ      ﾆｭｳﾘｮｸ2                            *
*--------------------------------------------------------------*
 220-INP-GRP03          SECTION.
     MOVE     "220-INP-GRP03"     TO   S-NAME.
*
     MOVE     "BODY02"            TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE      4010      TO   PROGRAM-STATUS
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF06
              MOVE      0         TO   ERR-FLG
              MOVE      2         TO   GR-NO
         WHEN ENT
              PERFORM   CLR-BODY1-RTN
              PERFORM   CLR-BODY2-RTN
              PERFORM   220-GRP03-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        PERFORM   220-SONZAI-CHECK
              END-IF
              IF        ERR-FLG   =    ZERO
                        MOVE      9    TO   GR-NO
              END-IF
         WHEN OTHER
              MOVE      1                   TO   ERR-FLG
     END-EVALUATE.
*
 220-INP-GRP03-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3                                                  *
*--------------------------------------------------------------*
 220-GRP03-CHECK-SEC    SECTION.
     MOVE     "220-CRP03-CHECK-SEC"    TO   S-NAME.
     MOVE     ZERO      TO        ERR-FLG.
*
*    ルート
     IF       ROUTEF   NOT NUMERIC
              MOVE     00    TO   ROUTEF
     END-IF.
     IF       ROUTET   NOT NUMERIC
              MOVE     99    TO   ROUTET
     END-IF.
     IF     ( ROUTEF    IS NUMERIC     AND
              ROUTET    IS NUMERIC     AND
              ROUTEF    >    ROUTET   )
              IF   ERR-FLG   =    ZERO
                   MOVE      7    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF ROUTEF
              MOVE     "R"   TO   EDIT-OPTION OF ROUTEF
              MOVE     "R"   TO   EDIT-OPTION OF ROUTET
     END-IF.
*    部門
     IF       SBUMON   NOT NUMERIC
              MOVE     00    TO   SBUMON
     END-IF.
     IF       EBUMON   NOT NUMERIC
              MOVE     99    TO   EBUMON
     END-IF.
     IF     ( EBUMON    NOT =     99    ) AND
            ( SBUMON    >    EBUMON     )
              IF   ERR-FLG   =    ZERO
                   MOVE      7    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF SBUMON
              MOVE     "R"   TO   EDIT-OPTION OF SBUMON
              MOVE     "R"   TO   EDIT-OPTION OF EBUMON
     END-IF.
*    発注日チェック
     IF    (( HACYY     IS NUMERIC     ) AND
            ( HACYY     >         ZERO )     ) OR
           (( HACMM     IS NUMERIC     ) AND
            ( HACMM     >         ZERO )     ) OR
           (( HACDD     IS NUMERIC     ) AND
            ( HACDD     >         ZERO )     )
              MOVE      HACYY     TO        WK-HACYY
              MOVE      HACMM     TO        WK-HACMM
              MOVE      HACDD     TO        WK-HACDD
              MOVE      WK-HACYMD TO        WK-HACYU
              MOVE     "2"        TO        LINK-IN-KBN
              MOVE      WK-HACYU  TO        LINK-IN-YMD8
              CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD8
              IF   LINK-OUT-RET   NOT =     ZERO
                   IF   ERR-FLG   =    ZERO
                        MOVE      9    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF HACYY
                   MOVE     "R"   TO   EDIT-OPTION OF HACYY
                   MOVE     "R"   TO   EDIT-OPTION OF HACMM
                   MOVE     "R"   TO   EDIT-OPTION OF HACDD
                   GO   TO   220-GRP03-CHECK-EXIT
              END-IF
     ELSE
              MOVE      ZERO           TO   WK-HACYU
     END-IF.
*    納品日チェック
     IF    (( NOUYY     IS NUMERIC     ) AND
            ( NOUYY     >         ZERO )     ) OR
           (( NOUMM     IS NUMERIC     ) AND
            ( NOUMM     >         ZERO )     ) OR
           (( NOUDD     IS NUMERIC     ) AND
            ( NOUDD     >         ZERO )     )
              MOVE      NOUYY     TO        WK-NOUYY
              MOVE      NOUMM     TO        WK-NOUMM
              MOVE      NOUDD     TO        WK-NOUDD
              MOVE      WK-NOUYMD TO        WK-NOUHIN
              MOVE     "2"        TO        LINK-IN-KBN
              MOVE      WK-NOUHIN TO        LINK-IN-YMD8
              CALL     "SKYDTCKB" USING     LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD8
              IF   LINK-OUT-RET   NOT =     ZERO
                   IF   ERR-FLG   =    ZERO
                        MOVE      9    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF NOUYY
                   MOVE     "R"   TO   EDIT-OPTION OF NOUYY
                   MOVE     "R"   TO   EDIT-OPTION OF NOUMM
                   MOVE     "R"   TO   EDIT-OPTION OF NOUDD
                   GO   TO   220-GRP03-CHECK-EXIT
              END-IF
     ELSE
              MOVE      ZERO           TO   WK-NOUHIN
     END-IF.
*    _番
*    IF     ( ETANA     NOT =    SPACE ) AND
*           ( STANA     >    ETANA     )
*             IF   ERR-FLG   =    ZERO
*                  MOVE      7    TO   ERR-FLG
*             END-IF
*             MOVE     "C"   TO   EDIT-CURSOR OF STANA
*             MOVE     "R"   TO   EDIT-OPTION OF STANA
*             MOVE     "R"   TO   EDIT-OPTION OF ETANA
*    END-IF.
*
 220-GRP03-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ファイル存在チェック　　                    *
*--------------------------------------------------------------*
 220-SONZAI-CHECK       SECTION.
     MOVE    "220-SONZAI-CHECK"  TO   S-NAME.
*
 220-SONZAI-CHECK-01.
     MOVE     SPACE          TO   BUL-REC.
     INITIALIZE                   BUL-REC.
*T↓
*    DISPLAY "JDATE     ="   JDATE UPON CONS.
*    DISPLAY "JTIME     ="   JTIME UPON CONS.
*    DISPLAY "TORICD    ="   TORICD UPON CONS.
*    DISPLAY "SOKO      ="   SOKO UPON CONS.
*    DISPLAY "STEN      ="   STEN UPON CONS.
*    DISPLAY "TORICD    ="   TORICD UPON CONS.
*    DISPLAY "ROUTEF    ="   ROUTEF UPON CONS.
*    DISPLAY "SBUMON    ="   SBUMON UPON CONS.
*    DISPLAY "WK-HACYMD2="   WK-HACYMD2 UPON CONS.
*    DISPLAY "WK-NOUYMD2="   WK-NOUYMD2 UPON CONS.
*----DISPLAY "STANA     ="   STANA UPON CONS.
*T↑
     MOVE     JDATE          TO   BUL-F001.
     MOVE     JTIME          TO   BUL-F002.
     MOVE     TORICD         TO   BUL-F003.
     MOVE     SOKO           TO   BUL-F004.
     MOVE     STEN           TO   BUL-FA01.
     MOVE     TORICD         TO   BUL-FA02.
     IF       TORICD  =  23631
              MOVE    2363   TO   BUL-FA02
     END-IF.
     MOVE     ROUTEF         TO   BUL-FA03.
     MOVE     SBUMON         TO   BUL-FA06.
     MOVE     WK-HACYMD2     TO   BUL-FA04.
     MOVE     WK-NOUYMD2     TO   BUL-FA05.
     MOVE     ZERO           TO   BUL-FB06.
     START    KTBULKF   KEY  >=   BUL-F001  BUL-F002
                                  BUL-F003  BUL-F004
                                  BUL-FA01  BUL-FA02
                                  BUL-FA03  BUL-FA06
                                  BUL-FA04  BUL-FA05
                                  BUL-FB06
         INVALID   KEY
              MOVE      11   TO   ERR-FLG
              MOVE     "C"        TO   EDIT-CURSOR OF SOKO
              MOVE     "R"        TO   EDIT-OPTION OF SOKO
              MOVE     "R"        TO   EDIT-OPTION OF STEN
              MOVE     "R"        TO   EDIT-OPTION OF ETEN
              MOVE     "R"        TO   EDIT-OPTION OF ROUTEF
              MOVE     "R"        TO   EDIT-OPTION OF ROUTET
              MOVE     "R"        TO   EDIT-OPTION OF SBUMON
              MOVE     "R"        TO   EDIT-OPTION OF EBUMON
              MOVE     "R"        TO   EDIT-OPTION OF HACYY
              MOVE     "R"        TO   EDIT-OPTION OF HACMM
              MOVE     "R"        TO   EDIT-OPTION OF HACDD
              MOVE     "R"        TO   EDIT-OPTION OF NOUYY
              MOVE     "R"        TO   EDIT-OPTION OF NOUMM
              MOVE     "R"        TO   EDIT-OPTION OF NOUDD
*             MOVE     "R"        TO   EDIT-OPTION OF STANA
*             MOVE     "R"        TO   EDIT-OPTION OF ETANA
              GO   TO   220-SONZAI-CHECK-EXIT
     END-START.
*
 220-SONZAI-CHECK-02.
     READ     KTBULKF
                AT END
                   MOVE      11   TO   ERR-FLG
                   MOVE     "C"   TO   EDIT-CURSOR OF SOKO
                   MOVE     "R"   TO   EDIT-OPTION OF SOKO
                   MOVE     "R"   TO   EDIT-OPTION OF STEN
                   MOVE     "R"   TO   EDIT-OPTION OF ETEN
                   MOVE     "R"   TO   EDIT-OPTION OF ROUTEF
                   MOVE     "R"   TO   EDIT-OPTION OF ROUTET
                   MOVE     "R"   TO   EDIT-OPTION OF SBUMON
                   MOVE     "R"   TO   EDIT-OPTION OF EBUMON
                   MOVE     "R"   TO   EDIT-OPTION OF HACYY
                   MOVE     "R"   TO   EDIT-OPTION OF HACMM
                   MOVE     "R"   TO   EDIT-OPTION OF HACDD
                   MOVE     "R"   TO   EDIT-OPTION OF NOUYY
                   MOVE     "R"   TO   EDIT-OPTION OF NOUMM
                   MOVE     "R"   TO   EDIT-OPTION OF NOUDD
*                  MOVE     "R"   TO   EDIT-OPTION OF STANA
*                  MOVE     "R"   TO   EDIT-OPTION OF ETANA
                   GO   TO   220-SONZAI-CHECK-EXIT
     END-READ.
*
 220-SONZAI-CHECK-03.
     IF ( JDATE     =    BUL-F001 ) AND
        ( JTIME     =    BUL-F002 ) AND
        ( TORICD    =    BUL-F003 ) AND
********( TORICD    =    BUL-FA02 )
        ( 2363      =    BUL-FA02 )
        CONTINUE
     ELSE
        MOVE  11  TO   ERR-FLG
        MOVE "C"  TO   EDIT-CURSOR OF SOKO
        MOVE "R"  TO   EDIT-OPTION OF SOKO
        MOVE "R"  TO   EDIT-OPTION OF STEN
        MOVE "R"  TO   EDIT-OPTION OF ETEN
        MOVE "R"  TO   EDIT-OPTION OF ROUTEF
        MOVE "R"  TO   EDIT-OPTION OF ROUTET
        MOVE "R"  TO   EDIT-OPTION OF SBUMON
        MOVE "R"  TO   EDIT-OPTION OF EBUMON
        MOVE "R"  TO   EDIT-OPTION OF HACYY
        MOVE "R"  TO   EDIT-OPTION OF HACMM
        MOVE "R"  TO   EDIT-OPTION OF HACDD
        MOVE "R"  TO   EDIT-OPTION OF NOUYY
        MOVE "R"  TO   EDIT-OPTION OF NOUMM
        MOVE "R"  TO   EDIT-OPTION OF NOUDD
*       MOVE "R"  TO   EDIT-OPTION OF STANA
*       MOVE "R"  TO   EDIT-OPTION OF ETANA
        GO        TO   220-SONZAI-CHECK-EXIT
     END-IF.
*
 220-SONZAI-CHECK-04.
     IF  SOKO   NOT =  SPACE
         IF     BUL-F004     =      SOKO
                CONTINUE
         ELSE
                MOVE   11   TO   ERR-FLG
                MOVE  "C"   TO   EDIT-CURSOR OF SOKO
                MOVE  "R"   TO   EDIT-OPTION OF SOKO
                MOVE  "R"   TO   EDIT-OPTION OF STEN
                MOVE  "R"   TO   EDIT-OPTION OF ETEN
                MOVE  "R"   TO   EDIT-OPTION OF ROUTEF
                MOVE  "R"   TO   EDIT-OPTION OF ROUTET
                MOVE  "R"   TO   EDIT-OPTION OF SBUMON
                MOVE  "R"   TO   EDIT-OPTION OF EBUMON
                MOVE  "R"   TO   EDIT-OPTION OF HACYY
                MOVE  "R"   TO   EDIT-OPTION OF HACMM
                MOVE  "R"   TO   EDIT-OPTION OF HACDD
                MOVE  "R"   TO   EDIT-OPTION OF NOUYY
                MOVE  "R"   TO   EDIT-OPTION OF NOUMM
                MOVE  "R"   TO   EDIT-OPTION OF NOUDD
*               MOVE  "R"   TO   EDIT-OPTION OF STANA
*               MOVE  "R"   TO   EDIT-OPTION OF ETANA
                GO          TO   220-SONZAI-CHECK-EXIT
         END-IF
     END-IF.
*
 220-SONZAI-CHECK-05.
     IF  STEN   IS     NUMERIC
         IF     BUL-FA01    >=      STEN
                CONTINUE
         ELSE
                MOVE   11   TO   ERR-FLG
                MOVE  "C"   TO   EDIT-CURSOR OF SOKO
                MOVE  "R"   TO   EDIT-OPTION OF SOKO
                MOVE  "R"   TO   EDIT-OPTION OF STEN
                MOVE  "R"   TO   EDIT-OPTION OF ETEN
                MOVE  "R"   TO   EDIT-OPTION OF ROUTEF
                MOVE  "R"   TO   EDIT-OPTION OF ROUTET
                MOVE  "R"   TO   EDIT-OPTION OF SBUMON
                MOVE  "R"   TO   EDIT-OPTION OF EBUMON
                MOVE  "R"   TO   EDIT-OPTION OF HACYY
                MOVE  "R"   TO   EDIT-OPTION OF HACMM
                MOVE  "R"   TO   EDIT-OPTION OF HACDD
                MOVE  "R"   TO   EDIT-OPTION OF NOUYY
                MOVE  "R"   TO   EDIT-OPTION OF NOUMM
                MOVE  "R"   TO   EDIT-OPTION OF NOUDD
*               MOVE  "R"   TO   EDIT-OPTION OF STANA
*               MOVE  "R"   TO   EDIT-OPTION OF ETANA
                GO          TO   220-SONZAI-CHECK-EXIT
         END-IF
     END-IF.
*
 220-SONZAI-CHECK-06.
     IF  ETEN   IS     NUMERIC
         IF     BUL-FA01    <=  ETEN
                CONTINUE
         ELSE
                MOVE   11   TO   ERR-FLG
                MOVE  "C"   TO   EDIT-CURSOR OF SOKO
                MOVE  "R"   TO   EDIT-OPTION OF SOKO
                MOVE  "R"   TO   EDIT-OPTION OF STEN
                MOVE  "R"   TO   EDIT-OPTION OF ETEN
                MOVE  "R"   TO   EDIT-OPTION OF ROUTEF
                MOVE  "R"   TO   EDIT-OPTION OF ROUTET
                MOVE  "R"   TO   EDIT-OPTION OF SBUMON
                MOVE  "R"   TO   EDIT-OPTION OF EBUMON
                MOVE  "R"   TO   EDIT-OPTION OF HACYY
                MOVE  "R"   TO   EDIT-OPTION OF HACMM
                MOVE  "R"   TO   EDIT-OPTION OF HACDD
                MOVE  "R"   TO   EDIT-OPTION OF NOUYY
                MOVE  "R"   TO   EDIT-OPTION OF NOUMM
                MOVE  "R"   TO   EDIT-OPTION OF NOUDD
*               MOVE  "R"   TO   EDIT-OPTION OF STANA
*               MOVE  "R"   TO   EDIT-OPTION OF ETANA
                GO          TO   220-SONZAI-CHECK-EXIT
         END-IF
     END-IF.
*
 220-SONZAI-CHECK-07.
     IF  ROUTEF IS     NUMERIC
         IF     BUL-FA03    >=  ROUTEF
                CONTINUE
         ELSE
                GO          TO   220-SONZAI-CHECK-02
         END-IF
     END-IF.
*
 220-SONZAI-CHECK-08.
     IF  ROUTET IS     NUMERIC
         IF     BUL-FA03    <=  ROUTET
                CONTINUE
         ELSE
                GO          TO   220-SONZAI-CHECK-02
         END-IF
     END-IF.
*
 220-SONZAI-CHECK-09.
     IF  SBUMON IS     NUMERIC
         IF     BUL-FA06    >=  SBUMON
                CONTINUE
         ELSE
                GO          TO   220-SONZAI-CHECK-02
         END-IF
     END-IF.
*
 220-SONZAI-CHECK-10.
     IF  EBUMON IS     NUMERIC
         IF     BUL-FA06    <=  EBUMON
                CONTINUE
         ELSE
                GO          TO   220-SONZAI-CHECK-02
         END-IF
     END-IF.
*
 220-SONZAI-CHECK-11.
     IF  ( WK-HACYU  IS  NUMERIC ) AND
         ( WK-HACYU  NOT = ZERO  )
         IF     BUL-FA04     =  WK-HACYMD2
                CONTINUE
         ELSE
                GO          TO   220-SONZAI-CHECK-02
         END-IF
     END-IF.
*
 220-SONZAI-CHECK-12.
     IF ( WK-NOUHIN IS  NUMERIC ) AND
        ( WK-NOUHIN NOT = ZERO  )
*T
*         DISPLAY "WK-NOUYMD2=" WK-NOUYMD2 UPON CONS
*         DISPLAY "BUL-FA05  =" BUL-FA05   UPON CONS
*T
         IF     BUL-FA05     =  WK-NOUYMD2
                CONTINUE
         ELSE
                GO          TO   220-SONZAI-CHECK-02
         END-IF
     END-IF.
*
 220-SONZAI-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ｶｸﾆﾝ ﾆｭｳﾘｮｸ                                 *
*--------------------------------------------------------------*
 230-INP-KKNN           SECTION.
     MOVE     "230-INP-KKNN"      TO   S-NAME.
     MOVE     "KKNN"         TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      99        TO   GR-NO
              MOVE      4010      TO   PROGRAM-STATUS
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF06
              MOVE      0         TO   ERR-FLG
              MOVE      3         TO   GR-NO
         WHEN ENT
              PERFORM   CLR-TAIL-RTN
              MOVE      10        TO   GR-NO
         WHEN OTHER
              MOVE      1         TO   ERR-FLG
     END-EVALUATE.
*
 230-INP-KKNN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾊﾟﾗﾒﾀ ｼｭﾂﾘｮｸ                                *
*--------------------------------------------------------------*
 240-PARA-OUT-SEC           SECTION.
     MOVE     "240-PARA-OUT-SEC"      TO   S-NAME.
*
     MOVE     JDATE               TO   PARA-OUT-JDATE.
     MOVE     JTIME               TO   PARA-OUT-JTIME.
     MOVE     TORICD              TO   PARA-OUT-TORICD.
*
     IF       SOKO      =     SPACE
              MOVE      SPACE     TO   PARA-OUT-SOKO
     ELSE
              MOVE      SOKO      TO   PARA-OUT-SOKO
     END-IF.
*
     IF       STEN  NOT  NUMERIC
              MOVE      ZERO      TO   PARA-OUT-STENPO
     ELSE
              MOVE      STEN      TO   PARA-OUT-STENPO
     END-IF.
*
     IF       ETEN      NOT  NUMERIC
     OR       ETEN      =    ZERO
              MOVE      99999     TO   PARA-OUT-ETENPO
     ELSE
              MOVE      ETEN      TO   PARA-OUT-ETENPO
     END-IF.
*
     IF       ROUTEF NOT  NUMERIC
              MOVE      ZERO      TO   PARA-OUT-SROUTE
     ELSE
              MOVE      ROUTEF    TO   PARA-OUT-SROUTE
     END-IF.
*
     IF       ROUTET    NOT  NUMERIC
     OR       ROUTET    =    ZERO
              MOVE      99        TO   PARA-OUT-EROUTE
     ELSE
              MOVE      ROUTET    TO   PARA-OUT-EROUTE
     END-IF.
*
     IF       SBUMON NOT  NUMERIC
     OR       SBUMON    =    ZERO
              MOVE      ZERO      TO   PARA-OUT-SBUMON
     ELSE
              MOVE      SBUMON    TO   PARA-OUT-SBUMON
     END-IF.
*
     IF       EBUMON    NOT  NUMERIC
     OR       EBUMON    =    ZERO
              MOVE      99        TO   PARA-OUT-EBUMON
     ELSE
              MOVE      EBUMON    TO   PARA-OUT-EBUMON
     END-IF.
*
*    MOVE     HACYY               TO   WK-HACYY.
*    MOVE     HACMM               TO   WK-HACMM.
*    MOVE     HACDD               TO   WK-HACDD.
*    MOVE     WK-HACYMD           TO   WK-HACYU.
*    MOVE     WK-HACYU            TO   PARA-OUT-HDATE.
     IF       WK-HACYU     IS NOT NUMERIC
              MOVE    ZERO        TO   PARA-OUT-HDATE
     ELSE
              MOVE    WK-HACYU    TO   PARA-OUT-HDATE
     END-IF.
*T
*    DISPLAY "PARA-OUT-HDATE=" PARA-OUT-HDATE UPON CONS.
*T
*
     IF       WK-NOUHIN    IS NOT NUMERIC
              MOVE    ZERO        TO   PARA-OUT-NDATE
     ELSE
              MOVE    WK-NOUHIN   TO   PARA-OUT-NDATE
     END-IF.
*T
*    DISPLAY "PARA-OUT-NDATE=" PARA-OUT-NDATE UPON CONS.
*T
*
     MOVE     99                  TO   GR-NO.
*
 240-PARA-OUT-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 900-DSP-READ           SECTION.
     MOVE     "900-DSP-READ"      TO   S-NAME.
***  MOVE     "SCRERE"       TO   DSP-GRP.
     MOVE     "SCREEN"       TO   DSP-GRP.
     IF       ERR-FLG   =    0
              MOVE      SPACE               TO   MSG
              MOVE      "D"      TO   EDIT-OPTION OF MSG
     ELSE
              MOVE      MSG-TBL (ERR-FLG)   TO   MSG
     END-IF.
     IF       GR-NO     =    1
              MOVE      GUIDE01   TO   GUIDE
     ELSE
              MOVE      GUIDE02   TO   GUIDE
     END-IF.
     MOVE     WK-SYSYMD           TO   SYSYMD.
     ACCEPT   SYS-TIME2      FROM TIME.
     MOVE     SYS-TIMEW           TO   SYSTIM.
*
     PERFORM  900-DSP-WRITE.
*
     IF       MSG  NOT  =    SPACE
              MOVE "AL"      TO   DSP-PRO
     ELSE
              MOVE "NE"      TO   DSP-PRO
     END-IF.
     MOVE     SPACE          TO   MSG.
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
     MOVE     SPACE          TO   DSP-PRO.
     WRITE    FSY13261.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF JDATE
                                  EDIT-CURSOR OF JTIME
                                  EDIT-CURSOR OF TORICD.
     MOVE     "M"            TO   EDIT-OPTION OF JDATE
                                  EDIT-OPTION OF JTIME
                                  EDIT-OPTION OF TORICD.
 CLR-HEAD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ１属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-BODY1-RTN          SECTION.
*
     MOVE     " "            TO   EDIT-CURSOR OF SOKO.
     MOVE     "M"            TO   EDIT-OPTION OF SOKO.
     MOVE     " "            TO   EDIT-CURSOR OF STEN.
     MOVE     "M"            TO   EDIT-OPTION OF STEN.
     MOVE     " "            TO   EDIT-CURSOR OF ETEN.
     MOVE     "M"            TO   EDIT-OPTION OF ETEN.
*
 CLR-BODY1-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＢＯＤＹ２属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-BODY2-RTN          SECTION.
*
     MOVE     " "            TO   EDIT-CURSOR OF ROUTEF
                                  EDIT-CURSOR OF ROUTET
                                  EDIT-CURSOR OF SBUMON
                                  EDIT-CURSOR OF EBUMON
                                  EDIT-CURSOR OF HACYY
                                  EDIT-CURSOR OF HACMM
                                  EDIT-CURSOR OF HACDD
                                  EDIT-CURSOR OF NOUYY
                                  EDIT-CURSOR OF NOUMM
                                  EDIT-CURSOR OF NOUDD.
*                                 EDIT-CURSOR OF STANA
*                                 EDIT-CURSOR OF ETANA.
     MOVE     "M"            TO   EDIT-OPTION OF ROUTEF
                                  EDIT-OPTION OF ROUTET
                                  EDIT-OPTION OF SBUMON
                                  EDIT-OPTION OF EBUMON
                                  EDIT-OPTION OF HACYY
                                  EDIT-OPTION OF HACMM
                                  EDIT-OPTION OF HACDD
                                  EDIT-OPTION OF NOUYY
                                  EDIT-OPTION OF NOUMM
                                  EDIT-OPTION OF NOUDD.
*                                 EDIT-OPTION OF STANA
*                                 EDIT-OPTION OF ETANA.
*
 CLR-BODY2-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＴＡＩＬ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-TAIL-RTN           SECTION.
     MOVE     " "        TO   EDIT-CURSOR OF KKNN.
     MOVE     "M"        TO   EDIT-OPTION OF KKNN.
 CLR-TAIL-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
