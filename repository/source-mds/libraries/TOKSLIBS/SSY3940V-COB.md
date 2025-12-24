# SSY3940V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3940V.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ新ＥＤＩ　　　　　　　　　　*
*    業務名　　　　　　　：　ナフコ出荷支援　　　　　　　　　　*
*    モジュール名　　　　：　新発注ＣＳＶデータ出力　　　　　  *
*    作成日／更新日　　　：　2016/09/01                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　発注書店舗ワーク・発注書商品明細　*
*                            ワークデータをＣＳＶデータとして　*
*                            出力する。　　　　　              *
*                            店舗ワークの出力方法を変更する。  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY3940V.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2016/09/01.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*発注書店舗ワーク
     SELECT   HCTPXXX1  ASSIGN    TO        DA-01-VI-HCTPXXX1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HTP-F01   HTP-F05
                                            HTP-F09   HTP-F06
                                            HTP-F07
                        FILE      STATUS    HTP-STATUS.
*発注ＣＳＶ（店舗）　　
     SELECT   HNCSVTEN  ASSIGN    TO        DA-01-HNCSVTEN
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    CTP-STATUS.
*発注書商品明細ワーク
     SELECT   HCSMXXX1  ASSIGN    TO        DA-01-VI-HCSMXXX1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HSM-F01   HSM-F05
                                            HSM-F07   HSM-F09
                        FILE      STATUS    HSM-STATUS.
*発注ＣＳＶ（明細）　　
     SELECT   HNCSVMEI  ASSIGN    TO        DA-01-HNCSVMEI
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    CSM-STATUS.
*ナフコ商品マスタ
     SELECT   NFSHOMS1  ASSIGN     TO      DA-01-VI-NFSHOMS1
                        ORGANIZATION       INDEXED
                        ACCESS     MODE    RANDOM
                        RECORD     KEY     SHO-F01
                        FILE    STATUS     SHO-STATUS.
*ナフコ店舗マスタ
     SELECT   NFTENMS1  ASSIGN     TO      DA-01-VI-NFTENMS1
                        ORGANIZATION       INDEXED
                        ACCESS     MODE    RANDOM
                        RECORD     KEY     TEN-F01  TEN-F02
                        FILE    STATUS     TEN-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*発注書店舗ワーク
******************************************************************
 FD  HCTPXXX1
                        LABEL RECORD   IS   STANDARD.
     COPY     HCTPXXX1  OF        XFDLIB
              JOINING   HTP  AS   PREFIX.
*
******************************************************************
*発注ＣＳＶ（店舗）
******************************************************************
 FD  HNCSVTEN            LABEL RECORD   IS   STANDARD
     BLOCK    CONTAINS   20        RECORDS.
     COPY     HNCSVTEN   OF        XFDLIB
              JOINING    CTP       PREFIX.
******************************************************************
*発注書商品明細ワーク
******************************************************************
 FD  HCSMXXX1
                        LABEL RECORD   IS   STANDARD.
     COPY     HCSMXXX1  OF        XFDLIB
              JOINING   HSM  AS   PREFIX.
*
******************************************************************
*発注ＣＳＶ（明細）
******************************************************************
 FD  HNCSVMEI            LABEL RECORD   IS   STANDARD
     BLOCK    CONTAINS   1         RECORDS.
     COPY     HNCSVMEI   OF        XFDLIB
              JOINING    CSM       PREFIX.
******************************************************************
*ナフコ商品マスタ
******************************************************************
 FD  NFSHOMS1
                        LABEL RECORD   IS   STANDARD.
     COPY     NFSHOMS1  OF        XFDLIB
              JOINING   SHO  AS   PREFIX.
******************************************************************
*ナフコ店舗マスタ
******************************************************************
 FD  NFTENMS1
                        LABEL RECORD   IS   STANDARD.
     COPY     NFTENMS1  OF        XFDLIB
              JOINING   TEN  AS   PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*ＳＴＡＴＵＳ領域
 01  WK-ST.
     03  HTP-STATUS          PIC  X(02).
     03  HSM-STATUS          PIC  X(02).
     03  CTP-STATUS          PIC  X(02).
     03  CSM-STATUS          PIC  X(02).
     03  SHO-STATUS          PIC  X(02).
     03  TEN-STATUS          PIC  X(02).
*フラグ領域
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  HCTPXXX1-END-FLG        PIC  X(03)     VALUE  SPACE.
 01  HCSMXXX1-END-FLG        PIC  X(03)     VALUE  SPACE.
 01  NFTENMS1-INV-FLG        PIC  X(03)     VALUE  SPACE.
*カウンター領域
 01  WK-CNT.
     03  RD-CNT              PIC  9(08)     VALUE  ZERO.
     03  RD-CNT2             PIC  9(08)     VALUE  ZERO.
     03  OUT-CNT             PIC  9(08)     VALUE  ZERO.
     03  OUT-CNT2            PIC  9(08)     VALUE  ZERO.
*ブレイク領域
 01  WK-HTP-F01              PIC  9(08)     VALUE  ZERO.
 01  WK-HTP-F05              PIC  X(02)     VALUE  SPACE.
 01  WK-HTP-F09              PIC  9(08)     VALUE  ZERO.
*添字
 01  IX                      PIC  9(03)     VALUE  ZERO.
*システム日付編集領域
 01  WK-AREA.
     03  SYS-DATE            PIC  9(06).
     03  SYS-DATEW           PIC  9(08).
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SSY3940V".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY3940V".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY3940V".
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
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " 店舗情報読込件数   = ".
         05  MSG-IN01        PIC  ZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-OUT.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " CSVデータ抽出件数  = ".
         05  MSG-OUT01       PIC  ZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-IN2.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " 明細情報読込件数   = ".
         05  MSG-IN02        PIC  ZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-OUT2.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " CSVデータ抽出件数  = ".
         05  MSG-OUT02       PIC  ZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
*ブレイクＫＥＹ領域
*01  WK-BREAK-KEY.
*    03  WK-K26              PIC  X(01)   VALUE  SPACE.
*日付変換サブルーチン用領域
 01  LINK-AREA.
     03  LINK-IN-KBN         PIC   X(01).
     03  LINK-IN-YMD6        PIC   9(06).
     03  LINK-IN-YMD8        PIC   9(08).
     03  LINK-OUT-RET        PIC   X(01).
     03  LINK-OUT-YMD8       PIC   9(08).
*
*LINKAGE                SECTION.
*01  PARA-KNOST              PIC   9(08).
*01  PARA-KNOED              PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
*PROCEDURE              DIVISION USING PARA-KNOST
*                                      PARA-KNOED.
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HCTPXXX1.
     MOVE      "HCTPXXX1"   TO   AB-FILE.
     MOVE      HTP-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HNCSVTEN.
     MOVE      "HNCSVTEN"   TO   AB-FILE.
     MOVE      CTP-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HCSMXXX1.
     MOVE      "HCSMXXX1"   TO   AB-FILE.
     MOVE      HSM-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HNCSVMEI.
     MOVE      "HNCSVMEI"   TO   AB-FILE.
     MOVE      CSM-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFSHOMS1.
     MOVE      "NFSHOMS1"   TO   AB-FILE.
     MOVE      SHO-STATUS   TO   AB-STS.
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
     OPEN     INPUT     HCTPXXX1  HCSMXXX1  NFSHOMS1  NFTENMS1.
     OPEN     OUTPUT    HNCSVTEN  HNCSVMEI.
     DISPLAY  MSG-START UPON CONS.
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
*発注書店舗ワーク読込
     PERFORM HCTPXXX1-READ-SEC.
*終了判定
     IF   HCTPXXX1-END-FLG  =  "END"
          DISPLAY "＃＃出力対象店舗無し＃＃" UPON CONS
          MOVE    "END"     TO    END-FLG
          MOVE     4010     TO    PROGRAM-STATUS
     ELSE
          MOVE    HTP-F01     TO WK-HTP-F01
          MOVE    HTP-F05     TO WK-HTP-F05
          MOVE    HTP-F09     TO WK-HTP-F09
     END-IF.
*
*発注書商品明細ワーク読込
     PERFORM HCSMXXX1-READ-SEC.
*終了判定
     IF   HCSMXXX1-END-FLG  =  "END"
          DISPLAY "＃＃出力対象明細無し＃＃" UPON CONS
          MOVE    "END"     TO    END-FLG
          MOVE     4010     TO    PROGRAM-STATUS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　発注書店舗ワーク読込　　　　　　　　　　　　　　　　　　　*
****************************************************************
 HCTPXXX1-READ-SEC    SECTION.
*
     MOVE    "HCTPXXX1-READ-SEC"    TO   S-NAME.
*
     READ     HCTPXXX1
              AT  END
                  MOVE     "END"    TO  HCTPXXX1-END-FLG
                  GO                TO  HCTPXXX1-READ-EXIT
     END-READ.
*
     ADD      1     TO     RD-CNT.
*
 HCTPXXX1-READ-EXIT.
     EXIT.
****************************************************************
*　　発注書商品明細ワーク読込　　　　　　　　　　　　　　　　　*
****************************************************************
 HCSMXXX1-READ-SEC    SECTION.
*
     MOVE    "HCSMXXX1-READ-SEC"    TO   S-NAME.
*
     READ     HCSMXXX1
              AT  END
                  MOVE     "END"    TO  HCSMXXX1-END-FLG
                  GO                TO  HCSMXXX1-READ-EXIT
     END-READ.
*
     ADD      1     TO     RD-CNT2.
*
 HCSMXXX1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
 MAIN-01.
     MOVE     SPACE          TO      CTP-REC.
*****INITIALIZE                      CTP-REC.
*
     PERFORM  VARYING  IX FROM 1 BY 1 UNTIL
            ( HCTPXXX1-END-FLG    =   "END" )
        OR  ( IX          > 500        )
*             発注ＣＳＶ（店舗）SET
              PERFORM  HNCSVTEN-SET-SEC
*             発注書店舗ワーク読込み
              PERFORM  HCTPXXX1-READ-SEC
     END-PERFORM.
*
 MAIN-02.
     MOVE     SPACE          TO      CSM-REC.
*****INITIALIZE                      CSM-REC.
*             発注ＣＳＶ（明細) SET
     PERFORM  HNCSVMEI-SET-SEC.
*    発注ＣＳＶ（明細) OUT
     WRITE    CSM-REC.
     ADD      1        TO OUT-CNT2
*    発注書商品明細ワーク読込み
     PERFORM  HCSMXXX1-READ-SEC.
     IF       HCSMXXX1-END-FLG    =   "END"
              MOVE    "END"       TO   END-FLG
              GO                  TO   MAIN-EXIT
     ELSE
              GO                  TO   MAIN-02
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*発注ＣＳＶ（店舗）SET
****************************************************************
 HNCSVTEN-SET-SEC     SECTION.
*
     MOVE     "HNCSVTEN-SET-SEC"  TO  S-NAME.
*
 HNCSVTEN-SET-01.
*****初期化
     MOVE    SPACE     TO   CTP-REC.
     INITIALIZE             CTP-REC.
*****管理番号
     MOVE    HTP-F01   TO   CTP-F01.
     MOVE    ","       TO   CTP-A01.
*****作場ＣＤ
     MOVE    HTP-F05   TO   CTP-F02.
     MOVE    ","       TO   CTP-A02.
*****店着日
     MOVE    HTP-F09   TO   CTP-F03.
     MOVE    ","       TO   CTP-A03.
*****店舗ＣＤ
     MOVE    HTP-F06   TO   CTP-F04.
     MOVE    ","       TO   CTP-A04.
*****店舗マスタ索引
     MOVE    137607    TO   TEN-F01.
     MOVE    HTP-F06   TO   TEN-F02.
     PERFORM NFTENMS1-READ-SEC.
     IF  NFTENMS1-INV-FLG = "INV"
         DISPLAY NC"＃店舗Ｍ異常（未存在）" " TENPO-CD = "
         HTP-F06 NC"　＃" UPON CONS
         MOVE 4000     TO   PROGRAM-STATUS
         STOP RUN
     END-IF.
*****店舗名
     MOVE    X"28"     TO   CTP-F051.
     MOVE    TEN-F05   TO   CTP-F05.
     MOVE    X"29"     TO   CTP-F052.
     MOVE    ","       TO   CTP-A05.
*****郵便番号
     MOVE    TEN-F07   TO   CTP-F06.
     MOVE    ","       TO   CTP-A06.
*****住所
     MOVE    X"28"     TO   CTP-F071.
     MOVE    TEN-F08   TO   CTP-F07(1:15).
     MOVE    TEN-F09   TO   CTP-F07(16:15).
     MOVE    X"29"     TO   CTP-F072.
     MOVE    ","       TO   CTP-A07.
*****電話番号
     MOVE    TEN-F10   TO   CTP-F08.
     MOVE    ","       TO   CTP-A08.
*****県ＣＤ
     MOVE    TEN-F12   TO   CTP-F09.
     MOVE    ","       TO   CTP-A08.
*****新発注ＣＳＶデータ（店舗）出力
     WRITE   CTP-REC.
     ADD     1         TO   OUT-CNT.
*
 HNCSVTEN-SET-EXIT.
     EXIT.
****************************************************************
*発注ＣＳＶ（明細)SET
****************************************************************
 HNCSVMEI-SET-SEC     SECTION.
*
     MOVE     "HNCSVMEI-SET-SEC"  TO  S-NAME.
*
 HNCSVMEI-SET-01.
*****管理番号
     MOVE    HSM-F01   TO   CSM-F01.
     MOVE    ","       TO   CSM-A01.
*****作場ＣＤ
     MOVE    HSM-F05   TO   CSM-F02.
     MOVE    ","       TO   CSM-A02.
*****店着日
     MOVE    HSM-F07   TO   CSM-F03.
     MOVE    ","       TO   CSM-A03.
*****相手商品ＣＤ
     MOVE    HSM-F09   TO   CSM-F05.
     MOVE    ","       TO   CSM-A05.
     MOVE   X"28"      TO   CSM-F061.
*    商品名,規格
     MOVE    HSM-F09     TO         SHO-F01.
     READ    NFSHOMS1
        INVALID
             MOVE      ALL NC"＊" TO   CSM-F06
             MOVE      ALL NC"＊" TO   CSM-F07
             MOVE      ALL "*"    TO   CSM-F04
             MOVE      ZERO       TO   CSM-F08
        NOT INVALID
             MOVE      SHO-F05    TO   CSM-F06
             MOVE      SHO-F06    TO   CSM-F07
             MOVE      SHO-F04    TO   CSM-F04
             MOVE      SHO-F09    TO   CSM-F08
     END-READ.
     MOVE   X"29"      TO   CSM-F062.
     MOVE    ","       TO   CSM-A06    CSM-A07    CSM-A08.
     MOVE    ","       TO   CSM-A04.
     MOVE   X"28"      TO   CSM-F071.
     MOVE   X"29"      TO   CSM-F072.

     PERFORM  VARYING  IX FROM 1 BY 1 UNTIL
              IX          > 500
              IF      HSM-F201(IX)  NOT = ZERO
                      MOVE  HSM-F201(IX) TO   CSM-F091(IX)
              END-IF
              MOVE    ","           TO   CSM-A09(IX)
     END-PERFORM.
*
 HNCSVMEI-SET-EXIT.
     EXIT.
****************************************************************
*　　ナフコ店舗マスタ読込　　　　　　　　　　　　　　　　　　　*
****************************************************************
 NFTENMS1-READ-SEC    SECTION.
*
     READ  NFTENMS1
           INVALID      MOVE  "INV"   TO  NFTENMS1-INV-FLG
           NOT  INVALID MOVE  SPACE   TO  NFTENMS1-INV-FLG
     END-READ.
*
 NFTENMS1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE     RD-CNT     TO      MSG-IN01.
     MOVE     RD-CNT2    TO      MSG-IN02.
     MOVE     OUT-CNT    TO      MSG-OUT01.
     MOVE     OUT-CNT2   TO      MSG-OUT02.
     DISPLAY  MSG-IN     UPON    CONS.
     DISPLAY  MSG-OUT    UPON    CONS.
     DISPLAY  MSG-IN2    UPON    CONS.
     DISPLAY  MSG-OUT2   UPON    CONS.
*
     CLOSE     NFSHOMS1  NFTENMS1.
     CLOSE     HCTPXXX1  HNCSVTEN.
     CLOSE     HCSMXXX1  HNCSVMEI.
*
     DISPLAY  MSG-END UPON CONS.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
