# SST0020I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SST0020I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　営利種子ストックＮＯ管理　　　　  *
*    業務名　　　　　　　：　営利種子ストックＮＯ管理　　　　  *
*    モジュール名　　　　：　ストックＮＯ管理データ再抽出指示  *
*    作成日／更新日　　　：　2020/07/09                        *
*    作成者／更新者　　　：　NAV TAKAHASHI                     *
*    処理概要　　　　　　：　画面より、バッチ_、出力倉庫、    *
*                            納品日の範囲を入力し、チェック    *
*                            後、パラメタへ渡す。              *
*    作成日／更新日　　　：　                                  *
*    作成者／更新者　　　：　                                  *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SST0020I.
 AUTHOR.                NAV.
 DATE-WRITTEN.          07/05/17.
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
*----<< 売上伝票ファイル >>--*
     SELECT   SHTDENF   ASSIGN         DA-01-VI-SHTDENLA
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  DEN-F46   DEN-F47
                                       DEN-F01   DEN-F48
                                       DEN-F02   DEN-F04
                                       DEN-F051  DEN-F07
                                       DEN-F112  DEN-F03
                        STATUS         SHTDENF-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         HTOKMS-ST.
*----<< 倉庫マスタ >>--*
     SELECT   ZSOKMS    ASSIGN         DA-01-VI-ZSOKMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SOK-F01
                        STATUS         ZSOKMS-ST.
*----<< 商品名称マスタ >>--*
     SELECT   HMEIMS    ASSIGN         DA-01-VI-MEIMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  MEI-F011  MEI-F0121
                                       MEI-F0122 MEI-F0123
                        STATUS         HMEIMS-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 表示ファイル >>--*
 FD  DSPFILE            LABEL     RECORD   IS   STANDARD.
     COPY     FST00201  OF        XMDLIB.
*----<< 売上伝票ファイル >>--*
 FD  SHTDENF            LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL     RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 倉庫マスタ >>--*
 FD  ZSOKMS            LABEL      RECORD   IS   STANDARD.
     COPY     ZSOKMS   OF         XFDLIB
              JOINING   SOK       PREFIX.
*----<< 商品名称マスタ >>--*
 FD  HMEIMS            LABEL      RECORD   IS   STANDARD.
     COPY     HMEIMS   OF         XFDLIB
              JOINING   MEI       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  ERR-FLG        PIC  9(02)    VALUE  ZERO.
     03  HMEIMS-INV-FLG PIC  X(03)    VALUE  SPACE.
     03  TAISYO-FLG     PIC  X(01)    VALUE  SPACE.

*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SHTDENF-ST        PIC  X(02).
 01  HTOKMS-ST         PIC  X(02).
 01  ZSOKMS-ST         PIC  X(02).
 01  HMEIMS-ST         PIC  X(02).
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
              NC"取引先コードが違います。".
     03  MSG05               PIC  N(20)  VALUE
              NC"売上伝票内に対象ＤＴが存在しません。".
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
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-NOUDT             PIC   9(08).
 01  PARA-DSOKO             PIC   X(02).
*
****************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO
                                       PARA-NOUDT
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
     DISPLAY  "### SST0020I DSPFILE ERROR " DSP-CNTL " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF   HTOKMS    ZSOKMS    DSPFILE   HMEIMS.
     STOP     RUN.
*----<< 売上伝票ファイル >>--*
 SHTDENF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SST0020I SHTDENF ERROR " SHTDENF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF   HTOKMS    ZSOKMS    DSPFILE   HMEIMS.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SST0020I HTOKMS ERROR " HTOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF   HTOKMS    ZSOKMS    DSPFILE   HMEIMS.
     STOP     RUN.
*----<< 倉庫マスタ >>--*
 ZSOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZSOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SST0020I ZSOKMS ERROR " ZSOKMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF   HTOKMS    ZSOKMS    DSPFILE   HMEIMS.
     STOP     RUN.
*----<< 商品名称マスタ >>--*
 HMEIMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HMEIMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SST0020I HMEIMS ERROR " HMEIMS-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     DISPLAY  SEC-NAME                 UPON CONS.
     CLOSE    SHTDENF   HTOKMS    ZSOKMS    DSPFILE   HMEIMS.
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
     DISPLAY  "*** SST0020I START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     SHTDENF.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     ZSOKMS.
     OPEN     INPUT     HMEIMS.
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
     CLOSE    SHTDENF.
     CLOSE    HTOKMS.
     CLOSE    ZSOKMS.
     CLOSE    HMEIMS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SST0020I END *** "
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
     MOVE     SPACE          TO   FST00201.
*
     PERFORM  CLR-HEAD-RTN.
     PERFORM  CLR-TAIL-RTN.
*
     MOVE    "SST0020I"      TO   PGID.
     MOVE    "FST00201"      TO   FORM.
*倉庫コードパラメタより倉庫コード入力チェック
     IF       PARA-DSOKO  =  "01"  OR  "88"
              MOVE    " "    TO   EDIT-STATUS OF SOKO
     ELSE
              MOVE      PARA-SOKO TO   SOK-F01 SOKO
              READ      ZSOKMS
                  INVALID  KEY
                        MOVE      ALL NC"＊" TO   SOKONM
              NOT INVALID  KEY
                        MOVE      SOK-F02    TO   SOKONM
              END-READ
              MOVE    "X"    TO   EDIT-STATUS OF SOKO
     END-IF.
*
     MOVE     SPACE          TO   DSP-CNTL.
     MOVE     "FST00201"     TO   DSP-FMT.
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
              IF        ERR-FLG   =    ZERO
                        MOVE      3    TO   ERR-FLG
              END-IF
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
     IF  TORICD  NOT  NUMERIC
     AND TORICD  =  ZERO
         IF   ERR-FLG   =    ZERO
              MOVE      4    TO   ERR-FLG
         END-IF
         MOVE     "C"   TO   EDIT-CURSOR OF TORICD
         MOVE     "R"   TO   EDIT-OPTION OF TORICD
     ELSE
         MOVE      SPACE     TO   TOK-REC
         INITIALIZE               TOK-REC
         MOVE      TORICD    TO   TOK-F01
         READ      HTOKMS
             INVALID
                   MOVE      SPACE     TO   TORINM
                   IF   ERR-FLG   =    ZERO
                        MOVE      4    TO   ERR-FLG
                   END-IF
                   MOVE     "C"   TO   EDIT-CURSOR OF TORICD
                   MOVE     "R"   TO   EDIT-OPTION OF TORICD
             NOT INVALID
                   MOVE      TOK-F02   TO   TORINM
         END-READ
     END-IF.
*    倉庫コードチェック
     IF     ( SOKO      NOT =     SPACE )
     AND    ( SOKO      NOT =     "00"  )
              MOVE      SOKO      TO   SOK-F01
              READ      ZSOKMS
                  INVALID  KEY
                        MOVE      SPACE     TO   SOKONM
                        IF   ERR-FLG   =    ZERO
                             MOVE      6    TO   ERR-FLG
                        END-IF
                        MOVE     "C"   TO   EDIT-CURSOR OF SOKO
                        MOVE     "R"   TO   EDIT-OPTION OF SOKO
              NOT INVALID  KEY
                        MOVE      SOK-F02   TO   SOKONM
                        MOVE      SOKO      TO   WK-SOKCD
              END-READ
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
         PERFORM  TAISYO-DATACHK
         IF  TAISYO-FLG =  SPACE
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
         END-IF
     END-IF.
*
 220-GRP01-CHECK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    対象データ存在チェック                                    *
*--------------------------------------------------------------*
 TAISYO-DATACHK         SECTION.
     MOVE     "TAISYO-DATACHK"    TO   S-NAME.
*
     MOVE      SPACE          TO   DEN-REC  TAISYO-FLG.
     INITIALIZE                    DEN-REC
     MOVE      JDATE          TO   DEN-F46
     MOVE      JTIME          TO   DEN-F47
     MOVE      TORICD         TO   DEN-F01
     MOVE      WK-SOKCD       TO   DEN-F48
     START     SHTDENF   KEY  >=   DEN-F46   DEN-F47
                                   DEN-F01   DEN-F48
                                   DEN-F02   DEN-F04
                                   DEN-F051  DEN-F07
                                   DEN-F112  DEN-F03
     INVALID   KEY
               GO             TO   TAISYO-DATACHK-EXIT
     END-START.
*
 TAISYO-DATACHK-010.
     READ  SHTDENF
           AT  END
           GO             TO   TAISYO-DATACHK-EXIT
     END-READ.
 TAISYO-DATACHK-020.
*バッチＮＯチェック
*****DISPLAY "KEY1 = " JDATE  " : " DEN-F46  UPON CONS.
*    DISPLAY "KEY2 = " JTIME  " : " DEN-F47  UPON CONS.
*****DISPLAY "KEY3 = " TORICD " : " DEN-F01  UPON CONS.
     IF  ( JDATE  =  DEN-F46  )
     AND ( JTIME  =  DEN-F47  )
     AND ( TORICD =  DEN-F01  )
           CONTINUE
     ELSE
           GO             TO   TAISYO-DATACHK-EXIT
     END-IF.
 TAISYO-DATACHK-030.
*倉庫ＣＤチェック
     IF  WK-SOKCD NOT =  SPACE
         IF   WK-SOKCD  NOT =  DEN-F48
              GO          TO   TAISYO-DATACHK-010
         END-IF
     END-IF.
 TAISYO-DATACHK-040.
*納品日チェック
     IF  NOUDT  NOT =  ZERO
         IF   NOUDT  NOT =  DEN-F112
              GO          TO   TAISYO-DATACHK-010
         END-IF
     END-IF.
 TAISYO-DATACHK-050.
*ストックＮＯ管理区分チェック
     PERFORM  HMEIMS-READ-SEC.
     IF  HMEIMS-INV-FLG  =  "INV"
         GO               TO   TAISYO-DATACHK-010
     ELSE
         IF  MEI-F97  NOT = 1
             GO           TO   TAISYO-DATACHK-010
         END-IF
     END-IF.
*
 TAISYO-DATACHK-060.
     MOVE    1            TO   TAISYO-FLG.
*
 TAISYO-DATACHK-EXIT.
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
     MOVE     JDATE               TO   PARA-JDATE.
     MOVE     JTIME               TO   PARA-JTIME.
     MOVE     TORICD              TO   PARA-TORICD.
     MOVE     SOKO                TO   PARA-SOKO.
     IF       WK-SOKCD  =      SPACE
              MOVE      SPACE     TO   PARA-SOKO
     END-IF.
     MOVE     NOUDT               TO   PARA-NOUDT.
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
     WRITE    FST00201.
 900-DSP-WRITE-EXIT.
     EXIT.
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
*--------------------------------------------------------------*
*    商品名称マスタ読込　　　　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 HMEIMS-READ-SEC        SECTION.
*
     MOVE    DEN-F1411    TO   MEI-F011.
     MOVE    DEN-F1412    TO   MEI-F012.
     READ  HMEIMS  INVALID     MOVE  "INV"  TO  HMEIMS-INV-FLG
                   NOT INVALID MOVE  SPACE  TO  HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
