# SSY1229B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY1229B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷　　　　　　　　　　　　　　  *
*    サブシステム　　　　：　コーナン　　　ＥＤＩ　　　　　　  *
*    モジュール名　　　　：　ケースＰＤラベル発行データ作成　　*
*    　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*    作成日／作成者　　　：　2021/01/28 INOUE                  *
*    処理概要　　　　　　：　条件に従いケースＰＤラベル発行用　*
*                            データ（ＣＳＶ）を作成する。　　　*
*    更新履歴                                                  *
*    更新日／更新者　　　：　                                  *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY1229B.
*                  流用:SSY1227B
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/01/28.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< ケースＰＤラベルファイル >>--*
     SELECT   KNCASEF   ASSIGN    TO        DA-01-VI-KNCASEL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
*                       ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       CAS-F001  CAS-F002
                                            CAS-F003  CAS-F004
                                            CAS-FA02  CAS-FA07
                                            CAS-FA08  CAS-FA03
                                            CAS-FA04  CAS-FA05
                                            CAS-FC01
                        FILE  STATUS   IS   CAS-STATUS.
*----<< 店舗マスタ >>----*
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52   TEN-F011
                        FILE  STATUS   IS   TEN-STATUS.
*----<< ケースＰＤラベル発行データ >>----*
     SELECT   KNCASEDT  ASSIGN    TO        DA-01-S-KNCASEDT
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    CSV-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    ケースＰＤラベルファイル
******************************************************************
 FD  KNCASEF            LABEL     RECORD   IS   STANDARD.
     COPY     KNCASEF   OF        XFDLIB
              JOINING   CAS       PREFIX.
******************************************************************
*    店舗マスタ
******************************************************************
 FD  TENMS1             LABEL     RECORD   IS   STANDARD.
     COPY     TENMS1    OF        XFDLIB
              JOINING   TEN  AS   PREFIX.
******************************************************************
*    ケースＰＤラベル発行データ
******************************************************************
 FD  KNCASEDT           LABEL     RECORD   IS   STANDARD.
     COPY     KNCASEDT  OF        XFDLIB
              JOINING   CSV  AS   PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP2-CNT           PIC  9(08)     VALUE  ZERO.
     03  WRT-CNT             PIC  9(08)     VALUE  ZERO.
     03  RWT-CNT             PIC  9(08)     VALUE  ZERO.
     03  KMK2-CNT            PIC  9(08)     VALUE  ZERO.
     03  KMS-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  KNCASEF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENLA-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  TENMS1-INV-FLG      PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
 01  IX                      PIC  9(05)     VALUE  ZERO.
 01  WK-PARA-IN-TORICD       PIC  9(08)     VALUE  ZERO.
*
*退避
 01  BK-C128-REC             PIC  X(128)    VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME           PIC   9(08)  VALUE  ZERO.
*日付の編集
 01  WK-HDATE.
     03  WK-HDATE1         PIC 9(02).
     03  WK-HDATE2         PIC 9(06).
 01  WK-NDATE.
     03  WK-NDATE1         PIC 9(02).
     03  WK-NDATE2         PIC 9(06).
*
*ブレイク項目
 01  BRK-F001              PIC  9(08)   VALUE ZERO.
 01  BRK-F002              PIC  9(04)   VALUE ZERO.
 01  BRK-F003              PIC  9(08)   VALUE ZERO.
 01  BRK-F004              PIC  X(02)   VALUE SPACE.
 01  BRK-FA02              PIC  9(04)   VALUE ZERO.
 01  BRK-FA07              PIC  9(06)   VALUE ZERO.
 01  BRK-FA08              PIC  9(02)   VALUE ZERO.
 01  BRK-FA03              PIC  9(02)   VALUE ZERO.
 01  BRK-FA04              PIC  9(06)   VALUE ZERO.
 01  BRK-FA05              PIC  9(06)   VALUE ZERO.
 01  BRK-FC01              PIC  X(06)   VALUE SPACE.
*
 01  WK-ST.
     03  CAS-STATUS        PIC  X(02).
     03  DEN-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  IN-STATUS         PIC  X(02).
     03  CSV-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY1229B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY1229B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY1229B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-IN-BUMON             PIC   X(04).
 01  PARA-IN-TANTOU            PIC   X(02).
 01  PARA-IN-JDATE             PIC   9(08).
 01  PARA-IN-JTIME             PIC   9(04).
 01  PARA-IN-TORICD            PIC   9(08).
 01  PARA-IN-SOKO              PIC   X(02).
 01  PARA-IN-STEN              PIC   9(05).
 01  PARA-IN-ETEN              PIC   9(05).
 01  PARA-IN-SROUTE            PIC   9(02).
 01  PARA-IN-EROUTE            PIC   9(02).
 01  PARA-IN-SBUMON            PIC   9(02).
 01  PARA-IN-EBUMON            PIC   9(02).
 01  PARA-IN-HDATE             PIC   9(08).
 01  PARA-IN-NDATE             PIC   9(08).
 01  PARA-IN-STANA             PIC   X(06).
 01  PARA-IN-ETANA             PIC   X(06).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING
                                  PARA-IN-BUMON
                                  PARA-IN-TANTOU
                                  PARA-IN-JDATE
                                  PARA-IN-JTIME
                                  PARA-IN-TORICD
                                  PARA-IN-SOKO
                                  PARA-IN-STEN
                                  PARA-IN-ETEN
                                  PARA-IN-SROUTE
                                  PARA-IN-EROUTE
                                  PARA-IN-SBUMON
                                  PARA-IN-EBUMON
                                  PARA-IN-HDATE
                                  PARA-IN-NDATE
                                  PARA-IN-STANA
                                  PARA-IN-ETANA.
*
******************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNCASEF.
     MOVE      "KNCASEL3 "   TO   AB-FILE.
     MOVE      CAS-STATUS    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TENMS1.
     MOVE      "TENMS1"     TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNCASEDT.
     MOVE      "KNCASEDT"   TO   AB-FILE.
     MOVE      CAS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     KNCASEF  TENMS1.
     OPEN     OUTPUT    KNCASEDT.
*
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
     MOVE     SPACE     TO        WK-INV-FLG SHTDENLA-INV-FLG
                                             TENMS1-INV-FLG.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*
     IF  PARA-IN-TORICD  =  23631
         MOVE    2363           TO   WK-PARA-IN-TORICD
     ELSE
         MOVE    PARA-IN-TORICD TO   WK-PARA-IN-TORICD
     END-IF.
*   システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*
*    ケースＰＤラベルファイル　スタート
*T
*    DISPLAY  "PARA-IN-JDATE  =" PARA-IN-JDATE  UPON CONS.
*    DISPLAY  "PARA-IN-JTIME  =" PARA-IN-JTIME  UPON CONS.
*    DISPLAY  "PARA-IN-TORICD =" PARA-IN-TORICD UPON CONS.
*    DISPLAY  "PARA-IN-SOKO   =" PARA-IN-SOKO   UPON CONS.
*    DISPLAY  "PARA-IN-STEN   =" PARA-IN-STEN   UPON CONS.
*    DISPLAY  "PARA-IN-ETEN   =" PARA-IN-ETEN   UPON CONS.
*    DISPLAY  "PARA-IN-SROUTE =" PARA-IN-SROUTE UPON CONS.
*    DISPLAY  "PARA-IN-EROUTE =" PARA-IN-EROUTE UPON CONS.
*    DISPLAY  "PARA-IN-SBUMON =" PARA-IN-SBUMON UPON CONS.
*    DISPLAY  "PARA-IN-EBUMON =" PARA-IN-EBUMON UPON CONS.
*    DISPLAY  "PARA-IN-HDATE  =" PARA-IN-HDATE  UPON CONS.
*    DISPLAY  "PARA-IN-NDATE  =" PARA-IN-NDATE  UPON CONS.
*    DISPLAY  "PARA-IN-STANA  =" PARA-IN-STANA  UPON CONS.
*    DISPLAY  "PARA-IN-ETANA  =" PARA-IN-ETANA  UPON CONS.
*T
     MOVE      SPACE           TO   CAS-REC.
     INITIALIZE                     CAS-REC.
     MOVE      PARA-IN-JDATE   TO   CAS-F001.
     MOVE      PARA-IN-JTIME   TO   CAS-F002.
     MOVE      PARA-IN-TORICD  TO   CAS-F003.
     MOVE      PARA-IN-SOKO    TO   CAS-F004.
     MOVE      PARA-IN-STEN    TO   CAS-FA02.
     MOVE      PARA-IN-TORICD  TO   CAS-FA07.
     IF        PARA-IN-TORICD  =  23631
               MOVE  2363      TO   CAS-FA07
     END-IF.
     MOVE      PARA-IN-SROUTE  TO   CAS-FA08.
     MOVE      PARA-IN-SBUMON  TO   CAS-FA03.
     MOVE      PARA-IN-HDATE   TO   WK-HDATE.
     MOVE      WK-HDATE2       TO   CAS-FA04.
     MOVE      PARA-IN-NDATE   TO   WK-NDATE.
     MOVE      WK-NDATE2       TO   CAS-FA05.
     MOVE      PARA-IN-STANA   TO   CAS-FC01.
     START     KNCASEF   KEY   >=   CAS-F001  CAS-F002
                                    CAS-F003  CAS-F004
                                    CAS-FA02  CAS-FA07
                                    CAS-FA08  CAS-FA03
                                    CAS-FA04  CAS-FA05
                                    CAS-FC01
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*
*    ケースＰＤラベルファイル読込み
     PERFORM KNCASEF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　ケースＰＤラベルファイル読込み　　　　　　　　　　*
****************************************************************
 KNCASEF-READ-SEC    SECTION.
*
     MOVE    "KNCASEF-READ-SEC"    TO  S-NAME.
*
     READ     KNCASEF
*             AT  END
         NEXT AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  KNCASEF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
 KNCASEF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"              TO   S-NAME.
*
*対象データチェック
*
 MAIN001.
     IF ( PARA-IN-JDATE     =    CAS-F001 ) AND
        ( PARA-IN-JTIME     =    CAS-F002 ) AND
        ( PARA-IN-TORICD    =    CAS-F003 )
        CONTINUE
     ELSE
        MOVE  "END"      TO   END-FLG
        GO               TO   MAIN-EXIT
     END-IF.
*
 MAIN002.
     IF  PARA-IN-SOKO   NOT =  SPACE
         IF     CAS-F004     =      PARA-IN-SOKO
                CONTINUE
         ELSE
                MOVE  "END"      TO   END-FLG
                GO               TO   MAIN-EXIT
         END-IF
     END-IF.
*
 MAIN003.
     IF  PARA-IN-NDATE  NOT = ZERO
         IF     CAS-FA05     =  WK-NDATE2
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
 MAIN004.
     IF  PARA-IN-STEN      IS     NUMERIC
         IF     CAS-FA02    >=    PARA-IN-STEN
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
 MAIN005.
     IF  PARA-IN-ETEN      IS     NUMERIC
         IF     CAS-FA02    <=    PARA-IN-ETEN
                CONTINUE
         ELSE
                GO          TO   MAIN999
        END-IF
     END-IF.
*
 MAIN006.
     IF  PARA-IN-TORICD IS     NUMERIC
*********IF     CAS-FA07     =  PARA-IN-TORICD
         IF     CAS-FA07     =  WK-PARA-IN-TORICD
                CONTINUE
         ELSE
                GO          TO   MAIN999
        END-IF
     END-IF.
*
 MAIN007.
     IF  PARA-IN-SROUTE IS     NUMERIC
         IF     CAS-FA08    >=  PARA-IN-SROUTE
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
 MAIN008.
     IF  PARA-IN-EROUTE IS     NUMERIC
         IF     CAS-FA08    <=  PARA-IN-EROUTE
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
 MAIN009.
     IF  PARA-IN-SBUMON IS     NUMERIC
         IF     CAS-FA03    >=  PARA-IN-SBUMON
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
 MAIN010.
     IF  PARA-IN-EBUMON IS     NUMERIC
         IF     CAS-FA03    <=  PARA-IN-EBUMON
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
 MAIN011.
     IF  PARA-IN-HDATE  NOT = ZERO
         IF     CAS-FA04     =  WK-HDATE2
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
 MAIN012.
     IF  PARA-IN-STANA  NOT = SPACE
         IF     CAS-FA03    >=  PARA-IN-STANA
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
*
 MAIN888.
*
     PERFORM KNCASEDT-WRITE-SEC.
     MOVE    CAS-F001                TO  BRK-F001.
     MOVE    CAS-F002                TO  BRK-F002.
     MOVE    CAS-F003                TO  BRK-F003.
     MOVE    CAS-F004                TO  BRK-F004.
     MOVE    CAS-FA02                TO  BRK-FA02.
     MOVE    CAS-FA07                TO  BRK-FA07.
     MOVE    CAS-FA08                TO  BRK-FA08.
     MOVE    CAS-FA03                TO  BRK-FA03.
     MOVE    CAS-FA04                TO  BRK-FA04.
     MOVE    CAS-FA05                TO  BRK-FA05.
     MOVE    CAS-FC01                TO  BRK-FC01.
*
 MAIN999.
*    ケースＰＤラベルファイル読込み
     PERFORM KNCASEF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　ケースＰＤラベル発行データ出力処理
****************************************************************
 KNCASEDT-WRITE-SEC  SECTION.
*
     MOVE     "KNCASEDT-WRITE-SEC" TO      S-NAME.
*
     MOVE      SPACE               TO      CSV-REC.
     INITIALIZE                            CSV-REC.
     MOVE      ","                 TO      CSV-F001K
                                           CSV-F002K
                                           CSV-F003K
                                           CSV-F004K
                                           CSV-FA02K
                                           CSV-FA03K
                                           CSV-FA04K
                                           CSV-FA05K
                                           CSV-FA06K
                                           CSV-FA07K
                                           CSV-FA08K
                                           CSV-FA14K
                                           CSV-FA15K
                                           CSV-FA16K
                                           CSV-FB01K
                                           CSV-FB02K
                                           CSV-FB03K
                                           CSV-FB05K
                                           CSV-FB06K
                                           CSV-FB07K
                                           CSV-FB08K
                                           CSV-FB09K
                                           CSV-FB10K
                                           CSV-FB11K
                                           CSV-FC01K
                                           CSV-FC02K
                                           CSV-FC03K
                                           CSV-FC04K
                                           CSV-FC05K
                                           CSV-FC06K
                                           CSV-FD01K
                                           CSV-FD02K
                                           CSV-FD03K
                                           CSV-FD04K
                                           CSV-FD05K
                                           CSV-FD06K
                                           CSV-FD07K
                                           CSV-FD08K
                                           CSV-FD09K
                                           CSV-FD10K
                                           CSV-FD11K
                                           CSV-FD12K
                                           CSV-FD13K
                                           CSV-FD14K
                                           CSV-FD15K
                                           CSV-FD16K
                                           CSV-FD17K
                                           CSV-FD18K
                                           CSV-FE01K
                                           CSV-FE02K
                                           CSV-FILK.
*    MOVE      X"28"               TO      CSV-FC01S
     MOVE      X"28"               TO      CSV-FC02S.
*    MOVE      X"29"               TO      CSV-FC01E
     MOVE      X"29"               TO      CSV-FC02E.
     MOVE      CAS-F001            TO      CSV-F001.
     MOVE      CAS-F002            TO      CSV-F002.
     MOVE      CAS-F003            TO      CSV-F003.
     MOVE      CAS-F004            TO      CSV-F004.
*
     MOVE      CAS-FA02            TO      CSV-FA02.
     MOVE      CAS-FA03            TO      CSV-FA03.
     MOVE      CAS-FA04            TO      CSV-FA04.
     MOVE      CAS-FA05            TO      CSV-FA05.
     MOVE      CAS-FA06            TO      CSV-FA06.
     MOVE      CAS-FA07            TO      CSV-FA07.
     MOVE      CAS-FA08            TO      CSV-FA08.
     MOVE      CAS-FA14            TO      CSV-FA14.
     MOVE      CAS-FA15            TO      CSV-FA15.
     MOVE      CAS-FA16            TO      CSV-FA16.
*
     MOVE      CAS-FB01            TO      CSV-FB01.
     MOVE      CAS-FB02            TO      CSV-FB02.
     MOVE      CAS-FB03            TO      CSV-FB03.
     MOVE      CAS-FB05            TO      CSV-FB05.
     MOVE      CAS-FB06            TO      CSV-FB06.
     MOVE      CAS-FB07            TO      CSV-FB07.
     MOVE      CAS-FB08            TO      CSV-FB08.
     MOVE      CAS-FB09            TO      CSV-FB09.
     MOVE      CAS-FB10            TO      CSV-FB10.
     MOVE      CAS-FB11            TO      CSV-FB11.
*
     MOVE      PARA-IN-TORICD      TO      TEN-F52.
     MOVE      CAS-FD05            TO      TEN-F011.
     READ      TENMS1
       INVALID
*              MOVE   ALL NC"＊"   TO      CSV-FC01
               MOVE   ALL   "＊"   TO      CSV-FC01
       NOT INVALID
*              MOVE   TEN-F02      TO      CSV-FC01
               MOVE   TEN-F04      TO      CSV-FC01
     END-READ.
     MOVE      NC"株式会社サカタのタネ"
                                   TO      CSV-FC02.
     MOVE      SYS-DATEW           TO      CSV-FC03.
     MOVE      WK-TIME(1:6)        TO      CSV-FC04.
     MOVE      PARA-IN-BUMON       TO      CSV-FC05.
     MOVE      PARA-IN-TANTOU      TO      CSV-FC06.
*
     MOVE      CAS-FD01            TO      CSV-FD01.
     MOVE      CAS-FD02            TO      CSV-FD02.
     MOVE      CAS-FD03            TO      CSV-FD03.
     MOVE      CAS-FD04            TO      CSV-FD04.
     MOVE      CAS-FD05            TO      CSV-FD05.
     MOVE      CAS-FD06            TO      CSV-FD06.
     MOVE      CAS-FD07            TO      CSV-FD07.
     MOVE      CAS-FD08            TO      CSV-FD08.
     MOVE      CAS-FD09            TO      CSV-FD09.
     MOVE      CAS-FD10            TO      CSV-FD10.
     MOVE      CAS-FD11            TO      CSV-FD11.
     MOVE      CAS-FD12            TO      CSV-FD12.
     MOVE      CAS-FD13            TO      CSV-FD13.
     MOVE      CAS-FD14            TO      CSV-FD14.
     MOVE      CAS-FD15            TO      CSV-FD15.
     MOVE      CAS-FD16            TO      CSV-FD16.
     MOVE      CAS-FD17            TO      CSV-FD17.
     MOVE      CAS-FD18            TO      CSV-FD18.
*
     MOVE      ZERO                TO      CSV-FE01.
     MOVE      ZERO                TO      CSV-FE02.
*
     WRITE     CSV-REC.
     ADD       1                   TO      WRT-CNT.
*
 KNCASEDT-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY  NC"ＰＤファイル"   "IN  = " READ-CNT  UPON CONS.
     DISPLAY  NC"発行データ　"   "OUT = " WRT-CNT   UPON CONS.
*    DISPLAY  NC"　　　　　　"   "RWT = " RWT-CNT   UPON CONS.
*
     CLOSE     KNCASEF  KNCASEDT  TENMS1.
*
     DISPLAY   MSG-END   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
