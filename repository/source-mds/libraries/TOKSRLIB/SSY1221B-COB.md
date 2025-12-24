# SSY1221B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY1221B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷　　　　　　　　　　　　　　  *
*    サブシステム　　　　：　コーナン　　　ＥＤＩ　　　　　　  *
*    モジュール名　　　　：　箱数ファイル（ＰＯＲ一括）作成　　*
*    　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*    作成日／作成者　　　：　2021/01/19 INOUE                  *
*    処理概要　　　　　　：　条件に従い、リスト発行対象　　　　*
*                            データを抽出する　　　　　　　　　*
*    更新履歴                                                  *
*    更新日／更新者　　　：　2021/10/25 INOUE                  *
*    変更概要　　　　　　：　非ＰＯＲフラグ転送追加　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY1221B.
*                  流用:SSY1211B SSY8723B
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/01/19.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 基本情報ファイル >>--*
     SELECT   KNJOHOF   ASSIGN    TO        DA-01-VI-KNJOHOL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
*                       ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JOH-F001  JOH-F002
                                            JOH-F003  JOH-F004
                                            JOH-FA02  JOH-FA07
                                            JOH-FA08  JOH-FA03
                                            JOH-FA04  JOH-FA05
                                            JOH-FC01
                        FILE  STATUS   IS   JOH-STATUS.
*----<< 売上伝票ファイル >>----*
     SELECT   SHTDENLA  ASSIGN    TO        DA-01-VI-SHTDENLA
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DEN-F46   DEN-F47
                                            DEN-F01   DEN-F48
                                            DEN-F02   DEN-F04
                                            DEN-F051  DEN-F07
                                            DEN-F112  DEN-F03
                        FILE  STATUS   IS   DEN-STATUS.
*----<< 店舗マスタ >>----*
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52   TEN-F011
                        FILE  STATUS   IS   TEN-STATUS.
*----<< 箱数ファイル >>----*
     SELECT   KNHAKOF   ASSIGN    TO        DA-01-VI-KNHAKOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       HAK-F001  HAK-F002
                                            HAK-F003  HAK-F004
                                            HAK-FA05
                        FILE      STATUS    HAK-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    基本情報ファイル
******************************************************************
 FD  KNJOHOF            LABEL     RECORD   IS   STANDARD.
     COPY     KNJOHOF   OF        XFDLIB
              JOINING   JOH       PREFIX.
******************************************************************
*    売上伝票ファイル
******************************************************************
 FD  SHTDENLA           LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
******************************************************************
*    店舗マスタ
******************************************************************
 FD  TENMS1             LABEL     RECORD   IS   STANDARD.
     COPY     TENMS1    OF        XFDLIB
              JOINING   TEN  AS   PREFIX.
******************************************************************
*    箱数ファイル
******************************************************************
 FD  KNHAKOF            LABEL     RECORD   IS   STANDARD.
     COPY     KNHAKOF   OF        XFDLIB
              JOINING   HAK  AS   PREFIX.
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
     03  KNHAKOF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENLA-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  TENMS1-INV-FLG      PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
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
*
 01  WK-ST.
     03  JOH-STATUS        PIC  X(02).
     03  DEN-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  IN-STATUS         PIC  X(02).
     03  HAK-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY1221B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY1221B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY1221B".
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
 01  PARA-IN-NDATE             PIC   9(08).
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
                                  PARA-IN-NDATE.
*
******************************************************************
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNJOHOF.
     MOVE      "KNJOHOL3 "   TO   AB-FILE.
     MOVE      JOH-STATUS    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENLA.
     MOVE      "SHTDENLA"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
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
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KNHAKOF.
     MOVE      "KNHAKOF"   TO   AB-FILE.
     MOVE      HAK-STATUS   TO   AB-STS.
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
     OPEN     INPUT     SHTDENLA  TENMS1.
     OPEN     I-O       KNJOHOF  KNHAKOF.
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
*   システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*
*    基本情報ファイル　スタート
*T
*    DISPLAY  "PARA-IN-JDATE  =" PARA-IN-JDATE UPON CONS.
*    DISPLAY  "PARA-IN-JTIME  =" PARA-IN-JTIME UPON CONS.
*    DISPLAY  "PARA-IN-TORICD =" PARA-IN-TORICD UPON CONS.
*    DISPLAY  "PARA-IN-SOKO   =" PARA-IN-SOKO   UPON CONS.
*    DISPLAY  "PARA-IN-NDATE  =" PARA-IN-NDATE   UPON CONS.
*T
     MOVE      SPACE           TO   JOH-REC.
     INITIALIZE                     JOH-REC.
     MOVE      PARA-IN-JDATE   TO   JOH-F001.
     MOVE      PARA-IN-JTIME   TO   JOH-F002.
     MOVE      PARA-IN-TORICD  TO   JOH-F003.
     MOVE      PARA-IN-SOKO    TO   JOH-F004.
     MOVE      ZERO            TO   JOH-FA02.
     MOVE      ZERO            TO   JOH-FA07.
     MOVE      ZERO            TO   JOH-FA08.
     MOVE      ZERO            TO   JOH-FA03.
     MOVE      ZERO            TO   JOH-FA04.
*    MOVE      PARA-IN-NDATE   TO   JOH-FA05.
     MOVE      ZERO            TO   JOH-FC01.
     MOVE      PARA-IN-NDATE   TO   WK-NDATE.
     MOVE      WK-NDATE2       TO   JOH-FA05.
     START     KNJOHOF   KEY   >=   JOH-F001  JOH-F002
                                    JOH-F003  JOH-F004
                                    JOH-FA02  JOH-FA07
                                    JOH-FA08  JOH-FA03
                                    JOH-FA04  JOH-FA05
                                    JOH-FC01
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*
*    基本情報ファイル読込み
     PERFORM KNJOHOF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 KNJOHOF-READ-SEC    SECTION.
*
     MOVE    "KNJOHOF-READ-SEC"    TO  S-NAME.
*
     READ     KNJOHOF
*             AT  END
         NEXT AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  KNJOHOF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
 KNJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"              TO   S-NAME.
*
*対象データチェック
 MAIN010.
*    一括のみ対象
     IF   JOH-FB12          =     0
          CONTINUE
     ELSE
          GO             TO   MAIN999
     END-IF.
 MAIN011.
     IF ( PARA-IN-JDATE     =    JOH-F001 ) AND
        ( PARA-IN-JTIME     =    JOH-F002 ) AND
        ( PARA-IN-TORICD    =    JOH-F003 )
        CONTINUE
     ELSE
        MOVE  "END"      TO   END-FLG
        GO               TO   MAIN-EXIT
     END-IF.
*
 MAIN012.
     IF  PARA-IN-SOKO   NOT =  SPACE
         IF     JOH-F004     =      PARA-IN-SOKO
                CONTINUE
         ELSE
                MOVE  "END"      TO   END-FLG
                GO               TO   MAIN-EXIT
         END-IF
     END-IF.
*
 MAIN013.
     IF  PARA-IN-NDATE  NOT = ZERO
         IF     JOH-FA05     =  WK-NDATE2
                CONTINUE
         ELSE
                GO          TO   MAIN999
         END-IF
     END-IF.
*
*売上伝票ファイル　検索
 MAIN024.
     MOVE     PARA-IN-JDATE(1:4)     TO  DEN-F461.
     MOVE     PARA-IN-JDATE(5:2)     TO  DEN-F462.
     MOVE     PARA-IN-JDATE(7:2)     TO  DEN-F463.
     MOVE     PARA-IN-JTIME(1:2)     TO  DEN-F471.
     MOVE     PARA-IN-JTIME(3:2)     TO  DEN-F472.
     MOVE     PARA-IN-TORICD         TO  DEN-F01.
     MOVE     JOH-F004               TO  DEN-F48.
     MOVE     JOH-FA06               TO  DEN-F02.
     MOVE     0                      TO  DEN-F04.
     MOVE     40                     TO  DEN-F051.
     MOVE     JOH-FA02               TO  DEN-F07.
     MOVE    "3"                     TO  LINK-IN-KBN.
     MOVE     JOH-FA05               TO  LINK-IN-YMD6.
     CALL    "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   DEN-F112
     ELSE
         MOVE    ZERO           TO   DEN-F112
     END-IF.
     MOVE     1                      TO  DEN-F03.
     READ     SHTDENLA
       INVALID
             DISPLAY NC"売上伝票ファイルなし！" UPON CONS
             DISPLAY NC"バッチ（日）＝" PARA-IN-JDATE UPON CONS
             DISPLAY NC"バッチ（時）＝" PARA-IN-JTIME UPON CONS
             DISPLAY NC"バッチ（取）＝" PARA-IN-TORICD UPON CONS
             DISPLAY NC"振分倉庫　　＝" JOH-F004       UPON CONS
             DISPLAY NC"伝票番号　　＝" JOH-FA06       UPON CONS
             DISPLAY NC"相殺区分　　＝" "0"            UPON CONS
             DISPLAY NC"伝区コード　＝" "40"           UPON CONS
             DISPLAY NC"店舗コード　＝" JOH-FA02       UPON CONS
             DISPLAY NC"納品日　　　＝" JOH-FA05       UPON CONS
             DISPLAY NC"行　　　　　＝" "1"            UPON CONS
             MOVE    4010            TO  PROGRAM-STATUS
             MOVE    "END"           TO  END-FLG
             GO                      TO  MAIN-EXIT
     END-READ.
*
     IF      DEN-F15    =   0
             GO                      TO  MAIN999
     END-IF.
*
     IF    ( JOH-F001   =   BRK-F001  ) AND
           ( JOH-F002   =   BRK-F002  ) AND
           ( JOH-F003   =   BRK-F003  ) AND
           ( JOH-F004   =   BRK-F004  ) AND
           ( JOH-FA02   =   BRK-FA02  ) AND
           ( JOH-FA07   =   BRK-FA07  ) AND
           ( JOH-FA08   =   BRK-FA08  ) AND
           ( JOH-FA03   =   BRK-FA03  ) AND
           ( JOH-FA04   =   BRK-FA04  ) AND
           ( JOH-FA05   =   BRK-FA05  )
             PERFORM    KNJOHOF-REWRITE-SEC
             GO         TO      MAIN999
     ELSE
             GO         TO      MAIN888
     END-IF.
*
 MAIN888.
*
     PERFORM KNHAKOF-WRITE-SEC.
     MOVE    JOH-F001                TO  BRK-F001.
     MOVE    JOH-F002                TO  BRK-F002.
     MOVE    JOH-F003                TO  BRK-F003.
     MOVE    JOH-F004                TO  BRK-F004.
     MOVE    JOH-FA02                TO  BRK-FA02.
     MOVE    JOH-FA07                TO  BRK-FA07.
     MOVE    JOH-FA08                TO  BRK-FA08.
     MOVE    JOH-FA03                TO  BRK-FA03.
     MOVE    JOH-FA04                TO  BRK-FA04.
     MOVE    JOH-FA05                TO  BRK-FA05.
     PERFORM KNJOHOF-REWRITE-SEC.
*    MOVE    JOH-REC                 TO  OUT-REC.
*    MOVE    DEN-F27D                TO  OUT-FC12.
*    WRITE   OUT-REC.
*    ADD     1                       TO  WRT-CNT.
*
 MAIN999.
*    基本情報ファイル読込み
     PERFORM KNJOHOF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　箱数ファイル出力処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 KNHAKOF-WRITE-SEC  SECTION.
*
     MOVE     "KNHAKOF-WRITE-SEC"  TO      S-NAME.
*
     MOVE      SPACE               TO      HAK-REC.
     INITIALIZE                            HAK-REC.
     MOVE      JOH-F0              TO      HAK-F0.
     MOVE      JOH-FA02            TO      HAK-FA01.
     MOVE      JOH-FA07            TO      HAK-FA02.
     MOVE      JOH-FA08            TO      HAK-FA03.
     MOVE      JOH-FA04            TO      HAK-FA04.
     MOVE      JOH-FA05            TO      HAK-FA05.
     MOVE      JOH-FA03            TO      HAK-FA06.
     MOVE      JOH-FA14            TO      HAK-FA07.
     MOVE      JOH-FA15            TO      HAK-FA08.
     MOVE      PARA-IN-TORICD      TO      TEN-F52.
     MOVE      JOH-FA02            TO      TEN-F011.
     READ      TENMS1
       INVALID
               MOVE   ALL NC"＊"   TO      HAK-FA09
       NOT INVALID
               MOVE   TEN-F02      TO      HAK-FA09
     END-READ.
     MOVE      NC"株式会社サカタのタネ"
                                   TO      HAK-FA10.
     MOVE      JOH-FB02            TO      HAK-FA11.
     MOVE      JOH-FB03            TO      HAK-FA12.
     MOVE      SYS-DATEW           TO      HAK-FC01.
     MOVE      WK-TIME(1:6)        TO      HAK-FC02.
     MOVE      PARA-IN-BUMON       TO      HAK-FC03.
     MOVE      PARA-IN-TANTOU      TO      HAK-FC04.
*↓2021/10/25
     MOVE      JOH-FD01            TO      HAK-FD01.
*↑2021/10/25
*
     WRITE     HAK-REC.
     ADD       1                   TO      WRT-CNT.
*
 KNHAKOF-WRITE-EXIT.
     EXIT.
****************************************************************
*　　基本情報ファイル更新　　　　　　　　　　　　　　　　　　　*
****************************************************************
 KNJOHOF-REWRITE-SEC  SECTION.
*
     MOVE     "KNJOHOF-REWRITE-SEC"   TO   S-NAME.
*
     MOVE      SYS-DATEW           TO      JOH-FC08.
     MOVE      WK-TIME(1:6)        TO      JOH-FC09.
     MOVE      PARA-IN-BUMON       TO      JOH-FC10.
     MOVE      PARA-IN-TANTOU      TO      JOH-FC11.
     MOVE      "1"                 TO      JOH-FC12.
     REWRITE   JOH-REC.
     ADD       1                   TO      RWT-CNT.
*
 KNJOHOF-REWRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY  NC"基本情報Ｆ" "IN  = " READ-CNT  UPON CONS.
     DISPLAY  NC"箱数Ｆ　　" "OUT = " WRT-CNT   UPON CONS.
     DISPLAY  NC"基本情報Ｆ" "RWT = " RWT-CNT   UPON CONS.
*
     CLOSE     KNJOHOF  KNHAKOF  SHTDENLA  TENMS1.
*
     DISPLAY   MSG-END   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
