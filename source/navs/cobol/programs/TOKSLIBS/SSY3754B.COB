****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコト新ＥＤＩシステム　　　　　*
*    業務名　　　　　　　：　出荷処理　　　　　　              *
*    モジュール名　　　　：　店舗テーブルファイル作成          *
*    作成日／更新日　　　：　10/10/07                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　出荷指示ファイルより店舗テーブル　*
*    　　　　　　　　　　　　ファイルを出力する。　　　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSY3754B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/10/07.
*
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
*
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.         CONSOLE   IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<ナフコ出荷指示Ｆ >>********************************
     SELECT   NFSYUKF           ASSIGN    TO   DA-01-VI-NFSYUKL3
                                ORGANIZATION   INDEXED
                                ACCESS  MODE   SEQUENTIAL
                                RECORD  KEY    NFS-F06  NFS-F01
                                               NFS-F07  NFS-F08
                                STATUS         NFS-STATUS.
*
*店舗マスタ
     SELECT  HTENMS    ASSIGN    TO        TENMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TEN-F52
                                           TEN-F011
                       FILE      STATUS    TEN-ST.
*****<<店舗テーブル    >>*************************************
     SELECT   NFTNTBL           ASSIGN    TO   FNTNTBL
                                STATUS         NFT-STATUS.
*                                                              *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = ナフコ出荷指示データ                               *
*--------------------------------------------------------------*
 FD  NFSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     NFSYUKF   OF        XFDLIB
              JOINING   NFS       PREFIX.
*
****************************************************************
*    FILE = 店舗マスタ                                         *
****************************************************************
 FD  HTENMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTENMS    OF   XFDLIB
                       JOINING   TEN       AS   PREFIX.
*--------------------------------------------------------------*
*    FILE = 店舗テーブル　　　　　　　　　　                   *
*--------------------------------------------------------------*
 FD  NFTNTBL           BLOCK CONTAINS 1    RECORDS.
 01  NFT-REC.
     03  NFT-F01        PIC       X(005).
     03  NFT-F02        PIC       9(003).
     03  NFT-FR         PIC       X(048).
*
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  NFS-STATUS                   PIC       X(02).
 01  TEN-ST                       PIC       X(02).
 01  NFT-STATUS                   PIC       X(02).
*
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD                   PIC       9(06)  VALUE  ZERO.
     03  SYS-DATEW                PIC       9(08)  VALUE  ZERO.
     03  SYS-DATE-R               REDEFINES SYS-DATEW.
         05  SYS-YY               PIC       9(04).
         05  SYS-MM               PIC       9(02).
         05  SYS-DD               PIC       9(02).
*
***** カウンタ
 01  READ-CNT                     PIC       9(07)  VALUE  ZERO.
 01  OUTPUT-CNT                   PIC       9(07)  VALUE  ZERO.
 01  SEQ-CNT                      PIC       9(04)  VALUE  ZERO.
***** 店舗テーブル
 01  BRK-KEY.
     03  BRK-F06                  PIC       9(05).
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3754B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3754B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSY3754B".
         05  FILLER               PIC       X(10)  VALUE
                       " ABEND ###".
*
     03  MSG-ABEND2.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-FL-ID            PIC       X(08).
         05  FILLER               PIC       X(04)  VALUE
                       " ST-".
         05  ERR-STCD             PIC       X(02).
         05  FILLER               PIC       X(04)  VALUE
                       " ###".
*
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
         05  FILLER         PIC   X(09)  VALUE " OUTPG= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE NFSYUKF.
     MOVE     "NFSYUKF "          TO        ERR-FL-ID.
     MOVE     NFS-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE NFTNTBL.
     MOVE     "NFTNTBL"           TO        ERR-FL-ID.
     MOVE     NFT-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC3         SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTENMS.
     DISPLAY     TEN-ST    UPON      CONS.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSY3754B-START         SECTION.
*
     MOVE   "SSY3754B-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
     PERFORM            END-SEC.
     STOP               RUN.
*
 SSY3754B-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     NFSYUKF.
     OPEN     INPUT     HTENMS.
     OPEN     OUTPUT    NFTNTBL.
     DISPLAY  MSG-START UPON CONS.
*
     PERFORM  NFSYUKF-RD-SEC.
     IF       END-FLG   =   "END"
              DISPLAY NC"＃対象データ無し＃" UPON CONS
              GO   TO    INIT-EXIT.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ナフコ出荷指示Ｆ読込　　　　　　
****************************************************************
 NFSYUKF-RD-SEC             SECTION.
*
     MOVE    "NFSYUKF-RD-SEC"    TO   S-NAME.
*
     READ     NFSYUKF
          AT END
              MOVE     "END"      TO        END-FLG
          NOT  AT  END
              ADD       1         TO        READ-CNT
     END-READ.
*
 NFSYUKF-RD-EXIT.
     EXIT.
*
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*初期化
     MOVE     SPACE               TO   NFT-REC.
     INITIALIZE                        NFT-REC.
*項目転送
**店舗マスタ検索
     IF    NFS-F06    NOT  =   BRK-F06
*          MOVE        NFS-F04          TO   TEN-F52
*          MOVE        NFS-F06          TO   TEN-F011
*          READ        HTENMS
*                      INVALID  KEY
*                          GO   TO  MAIN-0800
*                      NOT INVALID KEY
*                          ADD    1     TO   SEQ-CNT
*          END-READ
           ADD      1                   TO   SEQ-CNT
           MOVE     NFS-F06             TO   NFT-F01
           MOVE     SEQ-CNT             TO   NFT-F02
           WRITE    NFT-REC
           ADD      1                   TO   OUTPUT-CNT
           MOVE     NFS-F06             TO   BRK-F06
     END-IF.
*
 MAIN-0800.
*    次レコード読込み
     PERFORM  NFSYUKF-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
     DISPLAY "OUTPUT-CNT = " OUTPUT-CNT  UPON  CONS.
     DISPLAY "READ-CNT   = " READ-CNT    UPON  CONS.
*
     CLOSE    NFSYUKF   NFTNTBL  HTENMS.
     DISPLAY  MSG-END   UPON  CONS.
*
 END-EXIT.
     EXIT.
