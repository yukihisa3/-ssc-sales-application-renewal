****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　ＥＤＩＣ　　　　　　　　　　　　　*
*    モジュール名　　　　：　出荷確定データ作成指示　　　　　　*
*    作成日／更新日　　　：　2015/08/31                        *
*    作成者／更新者　　　：　ＮＡＶ宮下　　　　　　　　　　　　*
*    ＩＮＰＵＴ　　　　　：　倉庫コードＦ PARA-SOKCDF          *
*                        ：　代表倉庫ＣＤ PARA-DSOKO           *
*    ＯＵＴＰＵＴ　　　　：　取引先コード PARA-TORICD          *
*                        ：　受信日       PARA-DATE            *
*                        ：　受信時刻     PARA-TIME            *
*                        ：　倉庫コードＦ PARA-SOKCDF          *
*                        ：　倉庫コードＴ PARA-SOKCDT          *
*                        ：　納品日Ｆ     PARA-NOBIF           *
*                        ：　納品日Ｔ     PARA-NOBIT           *
*                        ：　代表倉庫ＣＤ PARA-DSOKO           *
*    処理概要　　　　　　：　オンライン出荷確定データの作成　　*
*                        ：　指示を行う。　　　　　　　　　　　*
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SED0061I.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2015/08/31.
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
*----<< EDIC発注MSG >>--*
     SELECT   EDJOHOF   ASSIGN         DA-01-VI-EDJOHOL2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  EDJ-F011  EDJ-F012
                                       EDJ-F013  EDJ-F02
                                       EDJ-F04   EDJ-F03
                                       EDJ-F05   EDJ-F06
                        STATUS         EDJOHOF-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FED00601  OF        XMDLIB.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 倉庫マスタ >>--*
 FD  ZSOKMS1            LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS1   OF        XFDLIB
              JOINING   SOK       PREFIX.
*----<< EDIC基本情報データ >>--*
 FD  EDJOHOF            LABEL     RECORD   IS   STANDARD.
     COPY     EDJOHOF   OF        XFDLIB
              JOINING   EDJ       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG        PIC  9(02)    VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  EDJOHOF-ST        PIC  X(02).
 01  HTOKMS-ST         PIC  X(02).
 01  ZSOKMS1-ST        PIC  X(02).
*
*----<< INVALID FLG >>--*
 01  TOK-INVALID-FLG    PIC  X(01).
 01  SOK-INVALID-FLG    PIC  X(01).
 01  EDJ-INVALID-FLG    PIC  X(01).
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
*
 01  WK-SOKCD           PIC  X(02)  VALUE  SPACE.
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
 01  GUIDE01       PIC  N(40)  VALUE   NC"ＰＦ５：終　了".
 01  GUIDE02       PIC  N(40)  VALUE
         NC"ＰＦ４：取　消　ＰＦ５：終　了　ＰＦ６：項目戻り".
*
 01  MSG-AREA.
     03  MSG01               PIC  N(20)  VALUE
              NC"ＰＦキーが違います".
     03  MSG02               PIC  N(20)  VALUE
              NC"バッチＮｏを入力して下さい。".
     03  MSG03               PIC  N(20)  VALUE
              NC"バッチＮｏに誤りがあります。".
     03  MSG04               PIC  N(20)  VALUE
              NC"取引先コードが違います。".
     03  MSG05               PIC  N(20)  VALUE
              NC"ＥＤＩＣ基本情報データに存在しません。".
     03  MSG06               PIC  N(20)  VALUE
              NC"倉庫が違います。".
     03  MSG07               PIC  N(20)  VALUE
              NC"対象データがありません。".
     03  MSG08               PIC  N(20)  VALUE
              NC"既に確定データ作成済みです。".
     03  MSG09               PIC  N(20)  VALUE
              NC"既にデータ送信済みです。".
     03  MSG10               PIC  N(20)  VALUE
              NC"納品日を正しく入力して下さい。".
     03  MSG11               PIC  N(20)  VALUE
              NC"取引先ＣＤは、資材ＣＤを入力して下さい。".
     03  MSG12               PIC  N(20)  VALUE
              NC"取引先ＣＤは、植物ＣＤを入力して下さい。".
*
 01  FILLER                  REDEFINES   MSG-AREA.
     03  MSG-TBL             PIC  N(20)  OCCURS      12.
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
 01  PARA-DATE              PIC   9(08).
 01  PARA-TIME              PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKCDF            PIC   X(02).
 01  PARA-SOKCDT            PIC   X(02).
 01  PARA-NOBIF             PIC   9(08).
 01  PARA-NOBIT             PIC   9(08).
 01  PARA-DSOKO             PIC   X(02).
*
****************************************************************
 PROCEDURE              DIVISION USING PARA-DATE
                                       PARA-TIME
                                       PARA-TORICD
                                       PARA-SOKCDF
                                       PARA-SOKCDT
                                       PARA-NOBIF
                                       PARA-NOBIT
                                       PARA-DSOKO.
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
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SED0061I DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    EDJOHOF   HTOKMS    ZSOKMS1    DSPFILE.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SED0061I HTOKMS ERROR " HTOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    EDJOHOF   HTOKMS    ZSOKMS1    DSPFILE.
     STOP     RUN.
*----<< 倉庫マスタ >>--*
 ZSOKMS1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZSOKMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SED0061I ZSOKMS1 ERROR " ZSOKMS1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    EDJOHOF   HTOKMS    ZSOKMS1    DSPFILE.
     STOP     RUN.
*----<< EDIC基本情報データ >>--*
 EDJOHOF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      EDJOHOF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SED0061I EDJOHOF ERROR " EDJOHOF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    EDJOHOF   HTOKMS    ZSOKMS1    DSPFILE.
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
*
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
     DISPLAY  "*** SED0061I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
*----<<FILE OPEN >>--*
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     EDJOHOF.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     ZSOKMS1.
*
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
*----<< ｶｸﾆﾝ ﾆｭｳﾘｮｸ >>-*
     PERFORM  230-INP-KKNN   UNTIL     GR-NO    NOT  =    9.
*----<< ﾊﾟﾗﾒﾀ ｼｭﾂﾘｮｸ >>-*
     PERFORM  240-PARA-SEC   UNTIL     GR-NO    NOT  =    10.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     MOVE    "300-END-RTN"        TO   S-NAME.
     CLOSE    DSPFILE.
     CLOSE    EDJOHOF.
     CLOSE    HTOKMS.
     CLOSE    ZSOKMS1.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SED0061I END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ﾃﾞｨｽﾌﾟﾚｰ  ｼｮｷ ﾋｮｳｼﾞ                         *
*--------------------------------------------------------------*
 210-DSP-INIT           SECTION.
     MOVE    "210-DSP-INIT"  TO   S-NAME.
     MOVE     SPACE          TO   FED00601.
*
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE    "SED0061I"      TO   PGID.
     MOVE    "FED00601"      TO   FORM.
*
*倉庫コードパラメタより倉庫コード入力チェック
     IF       PARA-DSOKO  =  "01"
              MOVE    " "    TO   EDIT-STATUS OF SOKO
     ELSE
              MOVE      PARA-SOKCDF    TO   SOK-F01 SOKO
              PERFORM 900-SOK-READ
              IF  SOK-INVALID-FLG = "1"
                  MOVE      SOK-F02    TO   SOKONM
              ELSE
                  MOVE      ALL NC"＊" TO   SOKONM
              END-IF
              MOVE    "X"    TO   EDIT-STATUS OF SOKO
     END-IF.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FED00601"     TO   DSP-FMT.
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
     MOVE     "GRP01"        TO   WK-GRP.
     PERFORM  900-DSP-READ.
*
     EVALUATE DSP-FNC
         WHEN PF05
              MOVE      4010      TO   PROGRAM-STATUS
              MOVE      99        TO   GR-NO
         WHEN ENT
              PERFORM   CLR-HEAD-RTN
              PERFORM   220-GRP01-CHECK-SEC
              IF        ERR-FLG   =    ZERO
                        MOVE      9    TO   GR-NO
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
*----<< ﾐﾆｭｳﾘｮｸ CHECK >>--*
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
*
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
              IF        ERR-FLG   =    ZERO
                        MOVE      3    TO   ERR-FLG
              END-IF
              MOVE     "C"        TO   EDIT-CURSOR OF JDATE
              MOVE     "R"        TO   EDIT-OPTION OF JDATE
              GO   TO   220-GRP01-CHECK-EXIT
         END-IF
     END-IF.
*
*    受信時間
     IF  JTIME     NOT NUMERIC
         MOVE      ZERO      TO   JTIME
     END-IF.
*
*    取引先チェック
     IF  TORICD  NOT  NUMERIC
     AND TORICD  =  ZERO
         IF   ERR-FLG   =    ZERO
              MOVE      4    TO   ERR-FLG
         END-IF
         MOVE     "C"   TO   EDIT-CURSOR OF TORICD
         MOVE     "R"   TO   EDIT-OPTION OF TORICD
     END-IF.
     IF  TORICD    NOT =     ZERO
         MOVE      SPACE     TO   TOK-REC
         INITIALIZE               TOK-REC
         MOVE      TORICD    TO   TOK-F01
         PERFORM 900-TOK-READ
         IF  TOK-INVALID-FLG = "1"
                   MOVE      TOK-F02   TO   TORINM
         ELSE
                   MOVE      SPACE     TO   TORINM
                   IF   ERR-FLG   =    ZERO
                        MOVE      4    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF TORICD
                   MOVE     "R"   TO   EDIT-OPTION OF TORICD
         END-IF
     END-IF.
*    倉庫コードチェック
     IF     ( SOKO      NOT =     SPACE )
     AND    ( SOKO      NOT =     "00"  )
              MOVE      SOKO      TO   SOK-F01
              PERFORM 900-SOK-READ
              IF  TOK-INVALID-FLG = "1"
                        MOVE      SOK-F02   TO   SOKONM
                        MOVE      SOKO      TO   WK-SOKCD
              ELSE
                        MOVE      SPACE     TO   SOKONM
                        IF   ERR-FLG   =    ZERO
                             MOVE      6    TO   ERR-FLG
                        END-IF
                        MOVE     "C"   TO   EDIT-CURSOR OF SOKO
                        MOVE     "R"   TO   EDIT-OPTION OF SOKO
              END-IF
     ELSE
              MOVE      SPACE          TO   WK-SOKCD
              MOVE      NC"全倉庫"     TO   SOKONM
     END-IF.
*    納品日チェック
     IF       NOUDT NOT NUMERIC
              MOVE      ZERO      TO   NOUDT
     ELSE
              IF   NOUDT  NOT =  ZERO
                   MOVE    "2"        TO        LINK-IN-KBN
                   MOVE     NOUDT     TO        LINK-IN-YMD8
                   CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                                LINK-IN-YMD6
                                                LINK-IN-YMD8
                                                LINK-OUT-RET
                                                LINK-OUT-YMD8
                   IF       LINK-OUT-RET   NOT =    ZERO
                            IF   ERR-FLG   =    ZERO
                                 MOVE      10   TO   ERR-FLG
                            END-IF
                            MOVE  "C"   TO  EDIT-CURSOR OF NOUDT
                            MOVE  "R"   TO  EDIT-OPTION OF NOUDT
                   END-IF
              END-IF
     END-IF.
*    売上伝票データ存在チェック
     IF  ERR-FLG        =    ZERO
         MOVE      SPACE          TO   EDJ-REC
         INITIALIZE                    EDJ-REC
         MOVE      JDATE          TO   EDJ-F011
         MOVE      JTIME          TO   EDJ-F012
         MOVE      TORICD         TO   EDJ-F013
         MOVE      SOKO           TO   EDJ-F02
         MOVE      NOUDT          TO   EDJ-F04
         START     EDJOHOF   KEY  >=   EDJ-F011  EDJ-F012
                                       EDJ-F013  EDJ-F02
                                       EDJ-F04   EDJ-F03
                                       EDJ-F05   EDJ-F06
              INVALID   KEY
                   MOVE      5    TO   ERR-FLG
                   MOVE     "C"   TO   EDIT-CURSOR OF JDATE
                   MOVE     "R"   TO   EDIT-OPTION OF JDATE
                   MOVE     "R"   TO   EDIT-OPTION OF JTIME
                   MOVE     "R"   TO   EDIT-OPTION OF TORICD
                   IF   WK-SOKCD  NOT =     SPACE
                        MOVE     "R"   TO   EDIT-OPTION OF SOKO
                   END-IF
                   IF   NOUDT     NOT =    ZERO
                        MOVE     "R"   TO   EDIT-OPTION OF NOUDT
                   END-IF
                   GO   TO   220-GRP01-CHECK-EXIT
              NOT INVALID
                   READ      EDJOHOF
                     AT END
                        MOVE      5    TO   ERR-FLG
                        MOVE     "C"   TO   EDIT-CURSOR OF JDATE
                        MOVE     "R"   TO   EDIT-OPTION OF JDATE
                        MOVE     "R"   TO   EDIT-OPTION OF JTIME
                        MOVE     "R"   TO   EDIT-OPTION OF TORICD
                        MOVE     "R"   TO   EDIT-OPTION OF SOKO
                        MOVE     "R"   TO   EDIT-OPTION OF NOUDT
                        GO   TO   220-GRP01-CHECK-EXIT
                     NOT AT END
                        IF ( JDATE     =    EDJ-F011 ) AND
                           ( JTIME     =    EDJ-F012 ) AND
                           ( TORICD    =    EDJ-F013 )
                             IF ( WK-SOKCD  NOT =    SPACE )
                                  IF   SOKO     =     EDJ-F02
                                       CONTINUE
                                  ELSE
                                       MOVE  5   TO   ERR-FLG
                                       MOVE "C"  TO
                                            EDIT-CURSOR OF JDATE
                                       MOVE "R"  TO
                                            EDIT-OPTION OF JDATE
                                            EDIT-OPTION OF JTIME
                                            EDIT-OPTION OF TORICD
                                            EDIT-OPTION OF SOKO
                                            EDIT-OPTION OF NOUDT
                                  END-IF
                             ELSE
                                  CONTINUE
                             END-IF
                             IF   NOUDT  =  EDJ-F04
                             OR   NOUDT  =  ZERO
                                  CONTINUE
                             ELSE
                                  MOVE  5   TO   ERR-FLG
                                  MOVE "C"  TO
                                        EDIT-CURSOR OF JDATE
                                  MOVE "R"  TO
                                        EDIT-OPTION OF JDATE
                                        EDIT-OPTION OF JTIME
                                        EDIT-OPTION OF TORICD
                                        EDIT-OPTION OF SOKO
                                        EDIT-OPTION OF NOUDT
                             END-IF
                        ELSE
                             MOVE      5    TO   ERR-FLG
                             MOVE "C"  TO   EDIT-CURSOR OF JDATE
                             MOVE "R"  TO   EDIT-OPTION OF JDATE
                             MOVE "R"  TO   EDIT-OPTION OF JTIME
                             MOVE "R"  TO   EDIT-OPTION OF TORICD
                             IF    WK-SOKCD NOT =    SPACE
                                   MOVE    "R"   TO
                                            EDIT-OPTION OF SOKO
                             END-IF
                             GO   TO   220-GRP01-CHECK-EXIT
                        END-IF
                   END-READ
         END-START
     END-IF.
*    チェックＯＫ時
     IF      ERR-FLG  =  ZERO
             IF    EDJ-F801 =  "1"
                   IF   ERR-FLG   =    ZERO
                        MOVE      9    TO   ERR-FLG
                   END-IF
                   MOVE "C"  TO   EDIT-CURSOR OF JDATE
                   MOVE "R"  TO   EDIT-OPTION OF JDATE
                                  EDIT-OPTION OF JTIME
                                  EDIT-OPTION OF TORICD
                                  EDIT-OPTION OF SOKO
                                  EDIT-OPTION OF NOUDT
             END-IF
     END-IF.
*
 220-GRP01-CHECK-EXIT.
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
              MOVE      4010      TO   PROGRAM-STATUS
              MOVE      99        TO   GR-NO
         WHEN PF04
              MOVE      0         TO   GR-NO
         WHEN PF06
              MOVE      1         TO   GR-NO
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
 240-PARA-SEC           SECTION.
     MOVE     "240-PARA-SEC"      TO   S-NAME.
*
     MOVE     JDATE               TO   PARA-DATE.
     MOVE     JTIME               TO   PARA-TIME.
     MOVE     TORICD              TO   PARA-TORICD.
     IF       SOKO      NOT =  SPACE
              MOVE      SOKO      TO   PARA-SOKCDF
              MOVE      SOKO      TO   PARA-SOKCDT
     ELSE
              MOVE      SPACE     TO   PARA-SOKCDF
              MOVE      "ZZ"      TO   PARA-SOKCDT
     END-IF.
     IF       NOUDT     NUMERIC
              MOVE      NOUDT     TO   PARA-NOBIF
              MOVE      NOUDT     TO   PARA-NOBIT
     ELSE
              MOVE      ZERO      TO   PARA-NOBIF
              MOVE      99999999  TO   PARA-NOBIT
     END-IF.
*
     MOVE     99                  TO   GR-NO.
*
 240-PARA-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL     ﾃﾞｨｽﾌﾟﾚｰ  READ                              *
*--------------------------------------------------------------*
 900-DSP-READ           SECTION.
     MOVE     "900-DSP-READ"      TO   S-NAME.
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
     WRITE    FED00601.
 900-DSP-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     MOVE     "900-TOK-READ"      TO   S-NAME.
     READ     HTOKMS
       INVALID
              MOVE      SPACE          TO   TOK-INVALID-FLG
              MOVE      SPACE          TO   TOK-F03
       NOT INVALID
              MOVE      "1"            TO   TOK-INVALID-FLG
     END-READ.
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    倉庫マスタ　 READ                            *
*--------------------------------------------------------------*
 900-SOK-READ           SECTION.
     MOVE     "900-SOK-READ"      TO   S-NAME.
     READ     ZSOKMS1
       INVALID
              MOVE      SPACE          TO   SOK-INVALID-FLG
              MOVE      SPACE          TO   SOK-F02
       NOT INVALID
              MOVE      "1"            TO   SOK-INVALID-FLG
     END-READ.
 900-SOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    EDIC基本情報データ  READ                     *
*--------------------------------------------------------------*
* 900-EDJ-READ           SECTION.
*     MOVE     "900-EDJ-READ"      TO   S-NAME.
*     READ     EDJOHOF
*       INVALID
*              MOVE      SPACE          TO   EDJ-INVALID-FLG
*       NOT INVALID
*              MOVE      "1"            TO   EDJ-INVALID-FLG
*     END-READ.
* 900-EDJ-READ-EXIT.
*     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ＨＥＡＤ　属性クリア　　　　　　　　　　　　*
*--------------------------------------------------------------*
 CLR-HEAD-RTN           SECTION.
     MOVE     " "            TO   EDIT-CURSOR OF JDATE
                                  EDIT-CURSOR OF JTIME
                                  EDIT-CURSOR OF TORICD
                                  EDIT-CURSOR OF NOUDT
                                  EDIT-CURSOR OF SOKO.
     MOVE     "M"            TO   EDIT-OPTION OF JDATE
                                  EDIT-OPTION OF JTIME
                                  EDIT-OPTION OF TORICD
                                  EDIT-OPTION OF NOUDT
                                  EDIT-OPTION OF SOKO.
 CLR-HEAD-EXIT.
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