# SSY5094B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY5094B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　トステムビバ　　　　　　　　　　　*
*    業務名　　　　　　　：　新ＥＤＩオンラインシステム　　　　*
*    モジュール名　　　　：　基本情報ファイル重複伝票番号対応  *
*    作成日／更新日　　　：　2010/06/10                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取ったパラメタより重複伝票Ｆを*
*                            読み、基本情報Ｆ内の伝票番号を修正*
*                            する。                           *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY5094B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/06/10.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上伝票データ
     SELECT   SHTDENF   ASSIGN    TO        DA-01-VI-SHTDENLI
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DEN-F46   DEN-F47
                                            DEN-F01   DEN-F02
                                            DEN-F07   DEN-F112
                                            DEN-F03
                        FILE      STATUS    DEN-STATUS.
*（共通）基本情報ファイル
     SELECT   KHJOHOF   ASSIGN    TO        DA-01-VI-KHJOHOL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       KHJ-F01   KHJ-F02
                                            KHJ-F03   KHJ-F04
                                            KHJ-F08   KHJ-F05
                                            KHJ-F06   KHJ-F07
                                            WITH DUPLICATES
                        FILE      STATUS    KHJ-STATUS.
*重複伝票ファイル
     SELECT   DENKANF   ASSIGN    TO        DA-01-VI-DENKANL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DKA-F01   DKA-F02
                                            DKA-F03   DKA-F04
                        FILE      STATUS    DKA-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*売上伝票ファイル
******************************************************************
 FD  SHTDENF
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*（共通）基本情報ファイル
******************************************************************
 FD  KHJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     KHJOHOF   OF        XFDLIB
              JOINING   KHJ       PREFIX.
******************************************************************
*重複伝票ファイル
******************************************************************
 FD  DENKANF            LABEL RECORD   IS   STANDARD.
     COPY     DENKANF   OF        XFDLIB
              JOINING   DKA       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*基本情報ファイル退避領域
     COPY   KHJOHOF  OF XFDLIB  JOINING   WKJ  AS   PREFIX.
*ＳＴＡＴＵＳ領域
 01  WK-ST.
     03  DEN-STATUS          PIC  X(02).
     03  KHJ-STATUS          PIC  X(02).
     03  DKA-STATUS          PIC  X(02).
*フラグ領域
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
*カウンター領域
 01  WK-CNT.
     03  RD-CNT              PIC  9(08)     VALUE  ZERO.
     03  WT-CNT              PIC  9(08)     VALUE  ZERO.
     03  KOS1-CNT            PIC  9(08)     VALUE  ZERO.
     03  WK-GYO-CNT          PIC  9(01)     VALUE  ZERO.
*INVALID領域
 01  WK-INV-FLG.
     03  KHSYUKF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  HJYOKEN-INV-FLG     PIC  X(03)     VALUE  SPACE.
*システム日付編集領域
 01  WK-AREA.
     03  SYS-DATE            PIC  9(06).
     03  SYS-DATEW           PIC  9(08).
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SSY5094B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY5094B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY5094B".
         05  FILLER          PIC  X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  AB-FILE         PIC  X(08).
         05  FILLER          PIC  X(06)  VALUE " ST = ".
         05  AB-STS          PIC  X(02).
         05  FILLER          PIC  X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  FILLER          PIC  X(07)  VALUE " SEC = ".
         05  S-NAME          PIC  X(30).
     03  MSG-IN.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  FILLER          PIC  X(09)  VALUE " INPUT = ".
         05  IN-CNT          PIC  9(06).
         05  FILLER          PIC  X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  FILLER          PIC  X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT         PIC  9(06).
         05  FILLER          PIC  X(05)  VALUE " *** ".
*ブレイクＫＥＹ領域
 01  WK-BREAK-KEY.
     03  WK-JDATE            PIC  9(08)   VALUE  ZERO.
     03  WK-JTIME            PIC  9(04)   VALUE  ZERO.
     03  WK-TOKCD            PIC  9(08)   VALUE  ZERO.
     03  WK-SOKCD            PIC  X(02)   VALUE  SPACE.
     03  WK-NOUDT            PIC  9(08)   VALUE  ZERO.
     03  WK-TENCD            PIC  9(05)   VALUE  ZERO.
     03  WK-DENNO            PIC  9(09)   VALUE  ZERO.
     03  WK-GYO              PIC  9(02)   VALUE  ZERO.
*日付変換サブルーチン用領域
 01  LINK-AREA.
     03  LINK-IN-KBN         PIC   X(01).
     03  LINK-IN-YMD6        PIC   9(06).
     03  LINK-IN-YMD8        PIC   9(08).
     03  LINK-OUT-RET        PIC   X(01).
     03  LINK-OUT-YMD8       PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-JDATE              PIC   9(08).
 01  PARA-JTIME              PIC   9(04).
 01  PARA-TORICD             PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENF.
     MOVE      "SHTDENLI"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   KHJOHOF.
     MOVE      "KHJOHOL1"   TO   AB-FILE.
     MOVE      KHJ-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DENKANF.
     MOVE      "DENKANL1"   TO   AB-FILE.
     MOVE      DKA-STATUS   TO   AB-STS.
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
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
     STOP  RUN.
*
 GENERAL-PROCESS-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     SHTDENF  DENKANF.
     OPEN     I-O       KHJOHOF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
     MOVE     SPACE     TO        WK-INV-FLG.
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
*重複伝票ファイルＳＴＡＲＴ
     MOVE     SPACE          TO      DKA-REC.
     INITIALIZE                      DKA-REC.
     MOVE     PARA-JDATE     TO      DKA-F01.
     MOVE     PARA-JTIME     TO      DKA-F02.
     MOVE     PARA-TORICD    TO      DKA-F03.
     MOVE     ZERO           TO      DKA-F04.
     START    DENKANF   KEY  >=      DKA-F01   DKA-F02
                                     DKA-F03   DKA-F04
         INVALID   KEY
              DISPLAY "＃＃伝票番号付替無し＃＃" UPON CONS
              DISPLAY  MSG-END UPON CONS
              STOP  RUN
     END-START.
*重複伝票ファイル読込
     PERFORM DENKANF-READ-SEC.
*終了判定
     IF   END-FLG  =  "END"
          DISPLAY "＃＃伝票番号付替無し＃＃" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 DENKANF-READ-SEC    SECTION.
*
     READ     DENKANF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  DENKANF-READ-EXIT
              NOT AT END
                  ADD       1       TO  RD-CNT
     END-READ.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  DKA-F01
     AND      PARA-JTIME  =  DKA-F02
     AND      PARA-TORICD =  DKA-F03
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  DENKANF-READ-EXIT
     END-IF.
*
 DENKANF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    売上伝票ファイル検索
     MOVE     DKA-F01             TO   DEN-F46.
     MOVE     DKA-F02             TO   DEN-F47.
     MOVE     DKA-F03             TO   DEN-F01.
     MOVE     DKA-F04             TO   DEN-F02.
     MOVE     1                   TO   DEN-F03.
     MOVE     0                   TO   DEN-F07.
     MOVE     0                   TO   DEN-F112.
     READ     SHTDENF    INVALID
              MOVE    "INV"       TO   SHTDENF-INV-FLG
              NOT  INVALID
              MOVE    SPACE       TO   SHTDENF-INV-FLG
     END-READ.
*
     IF       SHTDENF-INV-FLG  =  "INV"
              CONTINUE
     ELSE
**************基本情報ファイルを売上ファイルのキーで検索する
              PERFORM KHJOHOF-DENNO-SEC
     END-IF.
 MAIN010.
*重複伝票ファイル読込み
     PERFORM DENKANF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*出荷情報作成セクション
****************************************************************
 KHJOHOF-DENNO-SEC     SECTION.
*
     MOVE     "KHJOHOF-DENNO-SEC"  TO  S-NAME.
*売上ファイルのキーで基本情報ファイルをＳＴＡＲＴ
     MOVE      SPACE         TO    KHJ-REC.
     INITIALIZE                    KHJ-REC.
*
     MOVE      DEN-F46       TO    KHJ-F01.
     MOVE      DEN-F47       TO    KHJ-F02.
     MOVE      DEN-F01       TO    KHJ-F03.
     MOVE      DEN-F48       TO    KHJ-F04.
     MOVE      DEN-F112      TO    KHJ-F08.
     MOVE      DEN-F07       TO    KHJ-F05.
     MOVE      DKA-F05       TO    KHJ-F06.
     MOVE      ZERO          TO    KHJ-F07.
     START     KHJOHOF  KEY  >=    KHJ-F01  KHJ-F02  KHJ-F03
                                   KHJ-F04  KHJ-F08  KHJ-F05
                                   KHJ-F06  KHJ-F07
         INVALID   KEY
              DISPLAY "＃＃対象データ無し＃＃" UPON CONS
              DISPLAY  MSG-END UPON CONS
              GO             TO    KHJOHOF-DENNO-EXIT
     END-START.
*
 KHJOHOF-DENNO-010.
*
     READ  KHJOHOF  NEXT  AT  END
           GO             TO    KHJOHOF-DENNO-EXIT
     END-READ.
*
     IF    DEN-F46  =   KHJ-F01
     AND   DEN-F47  =   KHJ-F02
     AND   DEN-F01  =   KHJ-F03
     AND   DEN-F48  =   KHJ-F04
     AND   DEN-F112 =   KHJ-F08
     AND   DEN-F07  =   KHJ-F05
     AND   DKA-F05  =   KHJ-F06
***********ＷＫに退避して、基本ファイルを削除する。
*          MOVE    SPACE       TO   WKJ-REC
*          INITIALIZE               WKJ-REC
*          MOVE    KHJ-REC     TO   WKJ-REC
*          DELETE  KHJOHOF
*          MOVE    SPACE       TO   KHJ-REC
*          INITIALIZE               KHJ-REC
*          MOVE    WKJ-REC     TO   KHJ-REC
*          MOVE    DKA-F04     TO   KHJ-F06
***********DISPLAY "DKA-F04 = " DKA-F04 ":" KHJ-F06 UPON CONS
*           WRITE   KHJ-REC
           MOVE    DKA-F04     TO   KHJ-F06
           REWRITE   KHJ-REC
           ADD     1           TO   WT-CNT
           GO                  TO   KHJOHOF-DENNO-010
     END-IF.
*
 KHJOHOF-DENNO-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "DENKANF READ CNT = " RD-CNT    UPON CONS.
     DISPLAY "DEN-NO CHG   CNT = " WT-CNT    UPON CONS.
*
     CLOSE     SHTDENF  KHJOHOF  DENKANF.
*
     DISPLAY  MSG-END UPON CONS.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
