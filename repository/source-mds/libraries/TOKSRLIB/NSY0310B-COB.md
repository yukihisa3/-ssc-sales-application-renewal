# NSY0310B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NSY0310B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＤＣＭ仕入先統合　　　　　　　　　*
*    業務名　　　　　　　：　センター納入業務　　　　　　　　　*
*    モジュール名　　　　：　個口数記入一覧データ作成          *
*    作成日／更新日　　　：　2021/02/10                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取ったパラメタより対象データ  *
*                            をセンター納品データへ出力する。　*
**履歴**********************************************************
*    2018/02/19  高橋　　新規作成（ＳＳＹ９２０２Ｂコピー）　　*
*    0000/00/00  　　　　　　　　　　　　　　　　　　　　　　　*
*    0000/00/00　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NSY0310B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          21/02/10.
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
     SELECT   SHTDENF   ASSIGN    TO        DA-01-VI-SHTDENLA
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F46   DEN-F47
                                            DEN-F01   DEN-F48
                                            DEN-F02   DEN-F04
                                            DEN-F051  DEN-F07
                                            DEN-F112  DEN-F03
                        FILE      STATUS    DEN-STATUS.
*
*ダイキセンター納品データ
     SELECT   DNCENTF   ASSIGN    TO        DA-01-VI-DNCENTL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DKC-F01   DKC-F02
                                            DKC-F03   DKC-F04
                                            DKC-F05   DKC-F06
                                            DKC-F19   DKC-F07
                        FILE      STATUS    DKC-STATUS.
*店舗マスタ
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52  TEN-F011
                        FILE  STATUS   IS   TEN-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENF
                        LABEL RECORD   IS   STANDARD.
     COPY     DCMDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    ダイキセンター納品データ
******************************************************************
 FD  DNCENTF            LABEL RECORD   IS   STANDARD.
     COPY     DNCENTF   OF        XFDLIB
              JOINING   DKC       PREFIX.
******************************************************************
*    店舗マスタ
******************************************************************
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*ケーヨー情報格納領域
     COPY   DCMDENF  OF XFDLIB  JOINING   KEI  AS   PREFIX.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-HATYU-SYUBETSU       PIC  X(01)     VALUE  SPACE.
 01  WK-NOUDT-KIJYUN         PIC  9(08)     VALUE  ZERO.
 01  WK-DEN-F112             PIC  9(08)     VALUE  ZERO.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP-CNT            PIC  9(08)     VALUE  ZERO.
     03  DUB-CNT             PIC  9(08)     VALUE  ZERO.
     03  DKC-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  DNCENTF-INV-FLG     PIC  X(03)     VALUE  SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)     VALUE  SPACE.
 01  WK-KEKA                 PIC  9(03)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  DKC-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NSY0310B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NSY0310B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NSY0310B".
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
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENF.
     MOVE      "SHTDENL1"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DNCENTF.
     MOVE      "DNCENTF "   TO   AB-FILE.
     MOVE      DKC-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TENMS1.
     MOVE      "TENMS1  "   TO   AB-FILE.
     MOVE      DKC-STATUS   TO   AB-STS.
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
     OPEN     INPUT     SHTDENF  TENMS1.
     OPEN     I-O       DNCENTF.
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
*    売上伝票データスタート
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     PARA-JDATE     TO   DEN-F46.
     MOVE     PARA-JTIME     TO   DEN-F47.
     MOVE     PARA-TORICD    TO   DEN-F01.
     MOVE     PARA-SOKO      TO   DEN-F48.
*
     START    SHTDENF   KEY  >=   DEN-F46   DEN-F47
                                  DEN-F01   DEN-F48
                                  DEN-F02   DEN-F04
                                  DEN-F051  DEN-F07
                                  DEN-F112  DEN-F03
***2011.10.07 EN
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    売上伝票データ読込み
     PERFORM SHTDENF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 SHTDENF-READ-SEC    SECTION.
*
     READ     SHTDENF  NEXT
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  SHTDENF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
     INITIALIZE KEI-REC.
*
     MOVE     DEN-REC               TO  KEI-REC.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  DEN-F46
     AND      PARA-JTIME  =  DEN-F47
     AND      PARA-TORICD =  DEN-F01
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  SHTDENF-READ-EXIT
     END-IF.
*    店舗マスタ検索に変更
     MOVE        SPACE           TO        TEN-REC.
     INITIALIZE                            TEN-REC.
     MOVE        DEN-F01         TO        TEN-F52.
     MOVE        DEN-F07         TO        TEN-F011.
     READ    TENMS1
       INVALID
         MOVE    SPACE           TO        TEN-REC
         INITIALIZE                        TEN-REC
     END-READ.
*    オンライン伝票枚数（センター区分）
     IF       DEN-F40  NOT =     1
              ADD       1        TO   SKIP-CNT
              GO                 TO   SHTDENF-READ-SEC
     END-IF.
*店舗納品センター経由の場合、作成対象としない。
     IF       DEN-F40  =  1
     AND      KEI-M09  NOT =  SPACE
              ADD       1        TO   SKIP-CNT
              GO                 TO   SHTDENF-READ-SEC
     END-IF.
*
     IF       PARA-SOKO   =  SPACE
              GO                 TO   SHTDENF-READ-EXIT
     END-IF.
*    抽出条件のチェック
     IF       PARA-SOKO      =   DEN-F48
              GO                 TO   SHTDENF-READ-EXIT
     ELSE
              ADD       1        TO   SKIP-CNT
              GO                 TO   SHTDENF-READ-SEC
     END-IF.
*
 SHTDENF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
     MOVE     SPACE               TO   DKC-REC.
     INITIALIZE                        DKC-REC.
     MOVE     DEN-F46             TO   DKC-F01.   *>日付
     MOVE     DEN-F47             TO   DKC-F02.   *>時刻
     MOVE     DEN-F01             TO   DKC-F03.   *>取引先ＣＤ
     MOVE     DEN-F48             TO   DKC-F04.   *>倉庫ＣＤ
     MOVE     DEN-F112            TO   DKC-F05.   *>納品日
     MOVE     DEN-F07             TO   DKC-F06.   *>センターＣＤ
     MOVE     DEN-M17             TO   DKC-F07.   *>部門
     MOVE     DEN-M18             TO   DKC-F16.   *>発注種別変換
     MOVE     DEN-M03             TO   DKC-F19.   *>発注種別区分
*センター納品データ存在チェック
     READ     DNCENTF
         INVALID
                   PERFORM   DNCENTF-WRITE-SEC
         NOT INVALID
                   ADD  1    TO        DUB-CNT
                   GO   TO   MAIN010
     END-READ.
 MAIN010.
*    売上伝票データ読込み
     PERFORM SHTDENF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　ダイキセンター納品データ作成処理　　　　　　　　　　　　　*
****************************************************************
 DNCENTF-WRITE-SEC     SECTION.
*
     MOVE     "DNCENTF-WRITE-SEC"  TO  S-NAME.
*
*
     MOVE     SPACE               TO   DKC-REC.
     INITIALIZE                        DKC-REC.
     MOVE     DEN-F46             TO   DKC-F01.   *>日付
     MOVE     DEN-F47             TO   DKC-F02.   *>時刻
     MOVE     DEN-F01             TO   DKC-F03.   *>取引先ＣＤ
     MOVE     DEN-F48             TO   DKC-F04.   *>倉庫ＣＤ
     MOVE     DEN-F112            TO   DKC-F05.   *>納品日
     MOVE     DEN-F07             TO   DKC-F06.   *>センターＣＤ
     MOVE     DEN-M17             TO   DKC-F07.   *>部門
     MOVE     DEN-M18             TO   DKC-F16.   *>発注種別変換
     MOVE     DEN-M03             TO   DKC-F19.   *>発注種別区分
*
     WRITE     DKC-REC.
     ADD       1                   TO  DKC-CNT.
*
 DNCENTF-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "ｳﾘｱｹﾞﾃﾞﾝﾋﾟｮｳREAD CNT = " READ-CNT  UPON CONS.
     DISPLAY "ｾﾝﾀｰﾉｳﾋﾝ DT WT   CNT = " DKC-CNT   UPON CONS.
     DISPLAY "ｳﾘｱｹﾞﾃﾞﾝﾋﾟｮｳSKIP CNT = " SKIP-CNT  UPON CONS.
     DISPLAY "ﾀﾞﾌﾞﾘ       DUB  CNT = " DUB-CNT   UPON CONS.
*
     CLOSE     SHTDENF  DNCENTF  TENMS1.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
