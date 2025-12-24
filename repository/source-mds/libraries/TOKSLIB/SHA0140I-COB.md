# SHA0140I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SHA0140I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　自動発注サブシステム　　　　　　　*
*    モジュール名　　　　：　自動発注入力                      *
*    作成日／更新日　　　：　00/07/06                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　自動発注処理で作成した自動発注　　*
*                          ワークより発注するデータに確定区    *
*                          分に１をたてる処理を行う。          *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SHA0140I.
 AUTHOR.                NAV. Y.Y.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  画面ファイル  >>---*
     SELECT   DSPF      ASSIGN    TO        GS-DSPF
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        SYMBOLIC  DESTINATION    IS  "DSP"
                        PROCESSING MODE     IS   DSP-PROC
                        GROUP               IS   DSP-GROUP
                        FORMAT              IS   DSP-FORMAT
                        SELECTED  FUNCTION  IS   DSP-FUNC
                        DESTINATION-1       IS   DSP-WSNO
                        FILE      STATUS    IS   DSP-STATUS.
*
*---<<  自動発注ワーク  >>---*
     SELECT   AUTHACF   ASSIGN    TO        DA-01-VI-AUTHACL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   AUT-F01
                                                 AUT-F02
                                                 AUT-F031
                                                 AUT-F032
                                                 AUT-F04
                        FILE      STATUS    IS   AUT-STATUS.
*
*---<<  条件ファイル  >>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*
*---<<  仕入先マスタ  >>---*
     SELECT   ZSHIMS    ASSIGN    TO        DA-01-VI-ZSHIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHI-F01
                        FILE      STATUS    IS   SHI-STATUS.
*
*---<<  商品名称マスタ  >>---*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F011
                                                 MEI-F012
                        FILE      STATUS    IS   MEI-STATUS.
*
*---<<  倉庫マスタ  >>---*
     SELECT   ZSOKMS    ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SOK-F01
                        FILE      STATUS    IS   SOK-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  画面ファイル  >>---*
 FD  DSPF.
 01  DSP-REC            PIC  X(5000).
     COPY     FHA01401  OF        XMDLIB.
*---<<  自動発注ワーク  >>---*
 FD  AUTHACF.
     COPY     AUTHACF   OF        XFDLIB
              JOINING   AUT       PREFIX.
*---<<  条件ファイル  >>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*---<<  仕入先マスタ  >>---*
 FD  ZSHIMS.
     COPY     ZSHIMS    OF        XFDLIB
              JOINING   SHI       PREFIX.
*---<<  商品名称マスタ  >>---*
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*---<<  倉庫マスタ  >>---*
 FD  ZSOKMS.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  画面制御項目  ****
 01  DSP-CONTROL.
     03  DSP-PROC            PIC  X(02).
     03  DSP-GROUP           PIC  X(08).
     03  DSP-FORMAT          PIC  X(08).
     03  DSP-STATUS          PIC  X(02).
     03  DSP-FUNC            PIC  X(04).
     03  DSP-WSNO            PIC  X(08).
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  JYO-STATUS          PIC  X(02).
     02  SHI-STATUS          PIC  X(02).
     02  AUT-STATUS          PIC  X(02).
     02  MEI-STATUS          PIC  X(02).
     02  SOK-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  READ-FLG            PIC  X(01)  VALUE SPACE.
     02  MAIN-FLG            PIC  X(01)  VALUE SPACE.
     02  INVALID-FLG         PIC  9(01)  VALUE ZERO.
     02  AUT-FLG             PIC  9(01)  VALUE ZERO.
****  処理スイッチ  ****
 01  WK-AREA.
****  インデックス  ****
     02  IXA                 PIC  9(02).
     02  IXB                 PIC  9(02).
****  システム日付  ****
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE2                PIC  9(08).
*特販部名称編集
 01  HEN-TOKHAN-AREA.
     03  FILLER                   PIC  N(01)  VALUE  NC"（".
     03  HEN-TOKHAN               PIC  N(06)  VALUE  SPACE.
     03  FILLER                   PIC  N(01)  VALUE  NC"）".
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*
 01  HEN-TANA.
     03  HEN-TANA1                PIC  X(01)  VALUE  SPACE.
     03  FILLER                   PIC  X(01)  VALUE  "-".
     03  HEN-TANA2                PIC  X(03)  VALUE  SPACE.
     03  FILLER                   PIC  X(01)  VALUE  "-".
     03  HEN-TANA3                PIC  X(02)  VALUE  SPACE.
*
 01  SAV-KEY.
     03  SAV-SOKCD           PIC  X(02)   VALUE  SPACE.
     03  SAV-BKEY.
       05  SAV-BKEY-SHIIRE   PIC  X(08)   VALUE  SPACE.
       05  SAV-BKEY-SHOCD    PIC  X(08)   VALUE  SPACE.
       05  SAV-BKEY-HINT     PIC  X(08)   VALUE  SPACE.
       05  SAV-BKEY-TANA     PIC  X(06)   VALUE  SPACE.
     03  SAV-NKEY.
       05  SAV-NKEY-SHIIRE   PIC  X(08)   VALUE  SPACE.
       05  SAV-NKEY-SHOCD    PIC  X(08)   VALUE  SPACE.
       05  SAV-NKEY-HINT     PIC  X(08)   VALUE  SPACE.
       05  SAV-NKEY-TANA     PIC  X(06)   VALUE  SPACE.
     03  SAV-TANA-AREA.
       05  SAV-TANA-G        OCCURS    7.
         07  SAV-TANA        PIC  X(06)   VALUE  SPACE.
*
****  商品右詰め用エリア      ****
 01  WK-SHOCD.
     03  WK-SHO              PIC  X(01)   OCCURS  8.
*
****  ＰＦキーガイド  ***
 01  MSG-AREA.
     02  PMSG01            PIC N(07) VALUE
             NC"_取消　_終了".
     02  PMSG02            PIC N(19) VALUE
             NC"_取消　_終了　_戻り　_前頁　_次頁".
     02  PMSG03            PIC N(11) VALUE
             NC"_取消　_終了　_戻り".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SHA0140I".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  エラーメッセージコード  ***
 01  CODE-AREA.
     02  ERR-MSG-CD          PIC  9(02)  VALUE  ZERO.
****  エラーメッセージ  ***
 01  ERR-TAB.
     02  MSG-ERR1            PIC  N(20)  VALUE
             NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(20)  VALUE
             NC"倉庫コードを入力して下さい".
     02  MSG-ERR3            PIC  N(20)  VALUE
             NC"倉庫マスタに未登録です。".
     02  MSG-ERR4            PIC  N(20) VALUE
            NC"Ｙ，Ｈ，Ｂのいずれかで入力して下さい".
     02  MSG-ERR5            PIC  N(20) VALUE
            NC"ＹかＨで入力して下さい".
     02  MSG-ERR6            PIC  N(20) VALUE
            NC"Ｙで入力して下さい".
     02  MSG-ERR7            PIC  N(20)  VALUE
             NC"該当データがありません".
     02  MSG-ERR8            PIC  N(20)  VALUE
             NC"前頁がありません".
     02  MSG-ERR9            PIC  N(20)  VALUE
             NC"次頁がありません".
     02  MSG-ERR10           PIC  N(20)  VALUE
             NC"確定区分に誤りがあります。".
     02  MSG-ERR11           PIC  N(20)  VALUE
             NC"担当を入力して下さい。".
     02  MSG-ERR12           PIC  N(20)  VALUE
             NC"仕入先マスタに未登録です。".
 01  ERR-MSG-ALL     REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(20)
                             OCCURS 12  TIMES.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*----------------------------------------------------------*
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
*----------------------------------------------------------*
 PROCEDURE              DIVISION.
**
 DECLARATIVES.
 FILEERR-DSP            SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DSPF.
     MOVE     "DSPF    "       TO   ERR-FL-ID.
     MOVE      DSP-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-JYO            SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HJYOKEN.
     MOVE     "HJYOKEN"        TO   ERR-FL-ID.
     MOVE      JYO-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-ZSHI           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZSHIMS.
     MOVE     "ZSHIMS "        TO   ERR-FL-ID.
     MOVE      SHI-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-HMEI           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HMEIMS.
     MOVE     "HMEIMS  "       TO   ERR-FL-ID.
     MOVE      MEI-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-AUT            SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   AUTHACF.
     MOVE     "AUTHACF "       TO   ERR-FL-ID.
     MOVE      AUT-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SOK            SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZSOKMS.
     MOVE     "ZSOKMS  "       TO   ERR-FL-ID.
     MOVE      SOK-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 END     DECLARATIVES.
************************************************************
 SHA0140I-START         SECTION.
     PERFORM      INIT-SEC.
     PERFORM      MAIN-SEC
                  UNTIL     END-FLG  =    "END".
     PERFORM      END-SEC.
     STOP      RUN.
 SHA0140I-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     I-O       DSPF      AUTHACF.
     OPEN     INPUT     ZSHIMS    HJYOKEN  HMEIMS  ZSOKMS.
*
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*画面表示日付編集
     MOVE      SYS-DATE2(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE2(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE2(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*特販部名称編集
     MOVE    SPACE               TO        JYO-REC.
     INITIALIZE                            JYO-REC.
     MOVE    "99"                TO        JYO-F01.
     MOVE    "BUMON"             TO        JYO-F02.
     READ    HJYOKEN
       INVALID KEY
             MOVE NC"＊＊＊＊＊＊"   TO    HEN-TOKHAN
       NOT INVALID KEY
             MOVE JYO-F03            TO    HEN-TOKHAN
     END-READ.
     MOVE    HEN-TOKHAN-AREA      TO   TOKHAN.
*
     MOVE    "1"             TO   MAIN-FLG.
*
     MOVE     SPACE          TO   FHA01401.
     PERFORM       DSP-WRITE-SUB.
     PERFORM       EDIT-SET-HEAD.
     PERFORM       EDIT-SET-BODY.
 INIT-END.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 MAIN-SEC          SECTION.
     EVALUATE      MAIN-FLG
         WHEN      "1"  PERFORM   HEAD-SUB
         WHEN      "2"  PERFORM   BODY-SUB
         WHEN      "3"  PERFORM   KAKUNIN-SUB
         WHEN           OTHER     CONTINUE
     END-EVALUATE.
 MAIN-END.
     EXIT.
*==========================================================*
*      2.1       ヘッド部入力              MAIN-FLG=1      *
*==========================================================*
 HEAD-SUB            SECTION.
     PERFORM         MSG-SEC.
     MOVE     PMSG01         TO   PFMSG.
     PERFORM         DSP-WRITE-SUB.
     MOVE    "HEAD"          TO   DSP-GROUP.
     PERFORM         DSP-READ-SUB.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE    "1"    TO   MAIN-FLG
                        MOVE     SPACE TO   FHA01401
                        PERFORM             HEADDEL-SUB
                        PERFORM             BODYDEL-SUB
         WHEN   "F005"
                        MOVE     "END" TO   END-FLG
         WHEN   "E000"
                        PERFORM   BODYDEL-SUB
                        PERFORM   AUTHAC-DSP-SUB
                        IF   ERR-MSG-CD     =    ZERO
                             MOVE     "2"   TO   MAIN-FLG
                        END-IF
         WHEN    OTHER
                        MOVE      01   TO   ERR-MSG-CD
     END-EVALUATE.
 HEAD-END.
     EXIT.
*==========================================================*
*      2.1.1     明細セット処理                            *
*==========================================================*
 AUTHAC-DSP-SUB          SECTION.
*
     PERFORM  EDIT-SET-HEAD.
     MOVE     ZERO      TO   ERR-MSG-CD.
*    倉庫コード
     MOVE     SPACE     TO   SOKNM.
     MOVE     SPACE     TO   SAV-SOKCD.
     IF  SOKCD     =    SPACE
         IF   ERR-MSG-CD     =    ZERO
              MOVE      2    TO   ERR-MSG-CD
         END-IF
         MOVE     "R"   TO   EDIT-OPTION  OF  SOKCD
         MOVE     "C"   TO   EDIT-CURSOR  OF  SOKCD
     ELSE
         MOVE      SOKCD     TO   SOK-F01
         READ      ZSOKMS
              INVALID
                   IF   ERR-MSG-CD     =    ZERO
                        MOVE   3  TO   ERR-MSG-CD
                   END-IF
                   MOVE  "R"   TO   EDIT-OPTION  OF  SOKCD
                   MOVE  "C"   TO   EDIT-CURSOR  OF  SOKCD
              NOT INVALID
                   MOVE   SOK-F02   TO  SOKNM
                   MOVE   SOKCD     TO  SAV-SOKCD
         END-READ
     END-IF.
*    担当
     IF  TANTO     =    SPACE
         IF   ERR-MSG-CD     =    ZERO
              MOVE      11   TO   ERR-MSG-CD
         END-IF
         MOVE   "R"     TO   EDIT-OPTION  OF  TANTO
         MOVE   "C"     TO   EDIT-CURSOR  OF  TANTO
     END-IF.
*
*    仕入先のチェック
     MOVE     SPACE     TO   SINAME.
     IF       HSIIRE    =    SPACE
              CONTINUE
     ELSE
              PERFORM   SHI-READ1-SUB
**            IF  INVALID-FLG   =   "1"
**                IF    ERR-MSG-CD   =    ZERO
**                      MOVE    12   TO   ERR-MSG-CD
**                END-IF
**                MOVE   "R"     TO   EDIT-OPTION  OF  HSIIRE
**                MOVE   "C"     TO   EDIT-CURSOR  OF  HSIIRE
**            END-IF
     END-IF.
*
     IF  ERR-MSG-CD     NOT =     ZERO
         GO   TO   AUTHAC-DSP-EXIT
     END-IF.
*
     PERFORM  AUTHAC-START-SUB.
     IF       AUT-FLG   =    ZERO
              PERFORM   AUTHAC-READ-SUB
              IF   AUT-FLG   NOT =     ZERO
                   IF   ERR-MSG-CD     =    ZERO
                        MOVE      7    TO   ERR-MSG-CD
                   END-IF
                   MOVE   "R"     TO   EDIT-OPTION  OF  SOKCD
                   MOVE   "R"     TO   EDIT-OPTION  OF  TANTO
                   MOVE   "R"     TO   EDIT-OPTION  OF  HSIIRE
                   MOVE   "C"     TO   EDIT-CURSOR  OF  SOKCD
                   GO      TO     AUTHAC-DSP-EXIT
              END-IF
     ELSE
              IF   ERR-MSG-CD     =    ZERO
                   MOVE      7    TO   ERR-MSG-CD
              END-IF
              MOVE   "R"     TO   EDIT-OPTION  OF  SOKCD
              MOVE   "R"     TO   EDIT-OPTION  OF  TANTO
              MOVE   "R"     TO   EDIT-OPTION  OF  HSIIRE
              MOVE   "C"     TO   EDIT-CURSOR  OF  SOKCD
              GO      TO     AUTHAC-DSP-EXIT
     END-IF.
     MOVE     SPACE     TO   SAV-BKEY  SAV-NKEY.
     MOVE     SPACE     TO   SAV-TANA-AREA.
     PERFORM  VARYING   IXA  FROM  1  BY  1
              UNTIL   ( IXA       >    7 )  OR
                      ( AUT-FLG   =    1 )
         MOVE      AUT-F17        TO   KAKUTE(IXA)
         MOVE      AUT-F10        TO   HACSU (IXA)
         MOVE      AUT-F09        TO   HYOTEI(IXA)
         MOVE      AUT-F031       TO   SHOCD (IXA)
         MOVE      AUT-F032(1:5)  TO   HIN1  (IXA)
         MOVE      AUT-F032(6:2)  TO   HIN2  (IXA)
         MOVE      AUT-F032(8:1)  TO   HIN3  (IXA)
         PERFORM   MEI-READ-SUB
         MOVE      AUT-F04        TO   SAV-TANA(IXA)
         MOVE      AUT-F04(1:1)   TO   HEN-TANA1
         MOVE      AUT-F04(2:3)   TO   HEN-TANA2
         MOVE      AUT-F04(5:2)   TO   HEN-TANA3
         MOVE      HEN-TANA       TO   TANA  (IXA)
         MOVE      AUT-F05        TO   ZAIKO (IXA)
         MOVE      AUT-F06        TO   ANZEN (IXA)
         MOVE      AUT-F02        TO   SHIIRE(IXA)
         IF   AUT-F02   NOT =     SPACE
              PERFORM   SHI-READ-SUB
         END-IF
         MOVE      AUT-F14        TO   MTAN (IXA)
*
         IF        IXA  =    1
              MOVE AUT-F02        TO   SAV-BKEY-SHIIRE
              MOVE AUT-F031       TO   SAV-BKEY-SHOCD
              MOVE AUT-F032       TO   SAV-BKEY-HINT
              MOVE AUT-F04        TO   SAV-BKEY-TANA
         END-IF
         IF        IXA  =    7
              MOVE AUT-F02        TO   SAV-NKEY-SHIIRE
              MOVE AUT-F031       TO   SAV-NKEY-SHOCD
              MOVE AUT-F032       TO   SAV-NKEY-HINT
              MOVE AUT-F04        TO   SAV-NKEY-TANA
         END-IF
*
         PERFORM   AUTHAC-READ-SUB
     END-PERFORM.
*
 AUTHAC-DSP-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.1.1.1   商品名称マスタＳＴＡＲＴ                  *
*----------------------------------------------------------*
 AUTHAC-START-SUB        SECTION.
*
     MOVE    ZERO            TO   AUT-FLG.
     MOVE    SPACE           TO   AUT-REC.
     INITIALIZE                   AUT-REC.
     MOVE    SOKCD           TO   AUT-F01.
     MOVE    HSIIRE          TO   AUT-F02.
     START   AUTHACF    KEY  >=   AUT-F01
                                  AUT-F02
                                  AUT-F031
                                  AUT-F032
                                  AUT-F04
       INVALID      KEY
             MOVE       1         TO   AUT-FLG
       NOT INVALID  KEY
             MOVE       ZERO      TO   AUT-FLG
     END-START.
 AUTHAC-START-END.
     EXIT.
*----------------------------------------------------------*
*      2.1.1.2   商品名称マスタＲＥＡＤ                    *
*----------------------------------------------------------*
 AUTHAC-READ-SUB        SECTION.
*
     READ     AUTHACF   NEXT
         AT END
              MOVE      1    TO   AUT-FLG
         NOT AT END
              IF   AUT-F01   =    SAV-SOKCD
                   MOVE      ZERO      TO   AUT-FLG
                   IF   HSIIRE    NOT =     SPACE
                        IF   AUT-F02   NOT =     HSIIRE
                             MOVE      1    TO   AUT-FLG
                        END-IF
                   END-IF
              ELSE
                   MOVE      1    TO   AUT-FLG
              END-IF
     END-READ.
*
 AUTHAC-START-END.
     EXIT.
*==========================================================*
*      2.2       ボディ部入力              MAIN-FLG=2      *
*==========================================================*
 BODY-SUB            SECTION.
     PERFORM         MSG-SEC.
     MOVE     PMSG02         TO   PFMSG.
     PERFORM         DSP-WRITE-SUB.
     MOVE   "BODY"           TO   DSP-GROUP.
     PERFORM  VARYING  IXB   FROM  1  BY  1
              UNTIL    IXB   >     7
         IF   SHOCD(IXB)   =   SPACE
              MOVE   "X"   TO   EDIT-STATUS  OF  KAKUTE(IXB)
              MOVE   "X"   TO   EDIT-STATUS  OF  HACSU (IXB)
         END-IF
     END-PERFORM.
     PERFORM         DSP-READ-SUB.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE    "1"    TO   MAIN-FLG
                        MOVE     SPACE TO   FHA01401
                        PERFORM             HEADDEL-SUB
                        PERFORM             BODYDEL-SUB
         WHEN   "F005"
                        MOVE     "END" TO   END-FLG
         WHEN   "F006"
                        PERFORM             EDIT-SET-BODY
                        MOVE    "1"    TO   MAIN-FLG
         WHEN   "F011"
                        PERFORM   BODYCHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            PERFORM   AUTHAC-UPD-SUB
                            PERFORM   BEFORE-PAGE-SUB
                        END-IF
         WHEN   "F012"
                        PERFORM   BODYCHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            PERFORM   AUTHAC-UPD-SUB
                            PERFORM   NEXT-PAGE-SUB
                        END-IF
         WHEN   "E000"
                        PERFORM   BODYCHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            MOVE   "3"     TO   MAIN-FLG
                        END-IF
         WHEN    OTHER
                        MOVE   01          TO   ERR-MSG-CD
     END-EVALUATE.
 BODY-END.
     EXIT.
*----------------------------------------------------------*
*      2.4.1     ボディ部の入力チェック                    *
*----------------------------------------------------------*
 BODYCHK-SUB            SECTION.
     PERFORM  EDIT-SET-BODY.
     MOVE     ZERO      TO   ERR-MSG-CD.
*
     PERFORM  VARYING   IXA  FROM  1  BY  1
              UNTIL     IXA    >   7
         IF   SHOCD(IXA)     NOT =     SPACE
*             確定区分
              IF   KAKUTE(IXA)    NOT =     0  AND   1
                   IF   ERR-MSG-CD     =    ZERO
                        MOVE      10   TO   ERR-MSG-CD
                        MOVE  "C" TO  EDIT-CURSOR  OF KAKUTE(IXA)
                   END-IF
                   MOVE  "R"   TO     EDIT-OPTION  OF KAKUTE(IXA)
              END-IF
         END-IF
     END-PERFORM.
*
 BODYCHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.4.2     前頁処理                                  *
*----------------------------------------------------------*
 BEFORE-PAGE-SUB     SECTION.
*
     MOVE     ZERO      TO   ERR-MSG-CD.
     PERFORM  AUTHAC-START3-SUB.
     IF       AUT-FLG   =    ZERO
              PERFORM   AUTHAC-READ-SUB
              IF   AUT-FLG   NOT =     ZERO
                   IF   ERR-MSG-CD     =    ZERO
                        MOVE      8    TO   ERR-MSG-CD
                   END-IF
                   GO      TO     BEFORE-PAGE-EXIT
              END-IF
     ELSE
              IF   ERR-MSG-CD     =    ZERO
                   MOVE      8    TO   ERR-MSG-CD
              END-IF
              GO   TO   BEFORE-PAGE-EXIT
     END-IF.
     PERFORM  BODYDEL-SUB.
     MOVE     SPACE     TO   SAV-BKEY  SAV-NKEY.
     MOVE     SPACE     TO   SAV-TANA-AREA.
     PERFORM  VARYING   IXA  FROM  7  BY  -1
              UNTIL   ( IXA       =    0 )  OR
                      ( AUT-FLG   =    1 )
         MOVE      AUT-F17        TO   KAKUTE(IXA)
         MOVE      AUT-F10        TO   HACSU (IXA)
         MOVE      AUT-F09        TO   HYOTEI(IXA)
         MOVE      AUT-F031       TO   SHOCD (IXA)
         MOVE      AUT-F032(1:5)  TO   HIN1  (IXA)
         MOVE      AUT-F032(6:2)  TO   HIN2  (IXA)
         MOVE      AUT-F032(8:1)  TO   HIN3  (IXA)
         PERFORM   MEI-READ-SUB
         MOVE      AUT-F04        TO   SAV-TANA(IXA)
         MOVE      AUT-F04(1:1)   TO   HEN-TANA1
         MOVE      AUT-F04(2:3)   TO   HEN-TANA2
         MOVE      AUT-F04(5:2)   TO   HEN-TANA3
         MOVE      HEN-TANA       TO   TANA  (IXA)
         MOVE      AUT-F05        TO   ZAIKO (IXA)
         MOVE      AUT-F06        TO   ANZEN (IXA)
         MOVE      AUT-F02        TO   SHIIRE(IXA)
         IF   AUT-F02   NOT =     SPACE
              PERFORM   SHI-READ-SUB
         END-IF
         MOVE      AUT-F14        TO   MTAN (IXA)
*
         IF        IXA  =    1
              MOVE AUT-F02        TO   SAV-BKEY-SHIIRE
              MOVE AUT-F031       TO   SAV-BKEY-SHOCD
              MOVE AUT-F032       TO   SAV-BKEY-HINT
              MOVE AUT-F04        TO   SAV-BKEY-TANA
         END-IF
         IF        IXA  =    7
              MOVE AUT-F02        TO   SAV-NKEY-SHIIRE
              MOVE AUT-F031       TO   SAV-NKEY-SHOCD
              MOVE AUT-F032       TO   SAV-NKEY-HINT
              MOVE AUT-F04        TO   SAV-NKEY-TANA
         END-IF
*
         PERFORM   AUTHAC-READ-SUB
     END-PERFORM.
*
 BEFORE-PAGE-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.4.3     次頁処理                                  *
*----------------------------------------------------------*
 NEXT-PAGE-SUB     SECTION.
*
     MOVE     ZERO      TO   ERR-MSG-CD.
     IF       SAV-NKEY  =    SPACE
              MOVE      9    TO   ERR-MSG-CD
              GO   TO   NEXT-PAGE-EXIT
     END-IF.
     PERFORM  AUTHAC-START2-SUB.
     IF       AUT-FLG   =    ZERO
              PERFORM   AUTHAC-READ-SUB
              IF   AUT-FLG   NOT =     ZERO
                   IF   ERR-MSG-CD     =    ZERO
                        MOVE      9    TO   ERR-MSG-CD
                   END-IF
                   GO      TO     NEXT-PAGE-EXIT
              END-IF
     ELSE
              IF   ERR-MSG-CD     =    ZERO
                   MOVE      9    TO   ERR-MSG-CD
              END-IF
              GO   TO   NEXT-PAGE-EXIT
     END-IF.
     PERFORM  BODYDEL-SUB.
     MOVE     SPACE     TO   SAV-BKEY  SAV-NKEY.
     MOVE     SPACE     TO   SAV-TANA-AREA.
     PERFORM  VARYING   IXA  FROM  1  BY  1
              UNTIL   ( IXA       >    7 )  OR
                      ( AUT-FLG   =    1 )
         MOVE      AUT-F17        TO   KAKUTE(IXA)
         MOVE      AUT-F10        TO   HACSU (IXA)
         MOVE      AUT-F09        TO   HYOTEI(IXA)
         MOVE      AUT-F031       TO   SHOCD (IXA)
         MOVE      AUT-F032(1:5)  TO   HIN1  (IXA)
         MOVE      AUT-F032(6:2)  TO   HIN2  (IXA)
         MOVE      AUT-F032(8:1)  TO   HIN3  (IXA)
         PERFORM   MEI-READ-SUB
         MOVE      AUT-F04        TO   SAV-TANA(IXA)
         MOVE      AUT-F04(1:1)   TO   HEN-TANA1
         MOVE      AUT-F04(2:3)   TO   HEN-TANA2
         MOVE      AUT-F04(5:2)   TO   HEN-TANA3
         MOVE      HEN-TANA       TO   TANA  (IXA)
         MOVE      AUT-F05        TO   ZAIKO (IXA)
         MOVE      AUT-F06        TO   ANZEN (IXA)
         MOVE      AUT-F02        TO   SHIIRE(IXA)
         IF   AUT-F02   NOT =     SPACE
              PERFORM   SHI-READ-SUB
         END-IF
         MOVE      AUT-F14        TO   MTAN (IXA)
*
         IF        IXA  =    1
              MOVE AUT-F02        TO   SAV-BKEY-SHIIRE
              MOVE AUT-F031       TO   SAV-BKEY-SHOCD
              MOVE AUT-F032       TO   SAV-BKEY-HINT
              MOVE AUT-F04        TO   SAV-BKEY-TANA
         END-IF
         IF        IXA  =    7
              MOVE AUT-F02        TO   SAV-NKEY-SHIIRE
              MOVE AUT-F031       TO   SAV-NKEY-SHOCD
              MOVE AUT-F032       TO   SAV-NKEY-HINT
              MOVE AUT-F04        TO   SAV-NKEY-TANA
         END-IF
*
         PERFORM   AUTHAC-READ-SUB
     END-PERFORM.
*
 NEXT-PAGE-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.4.3     商品名称マスタＳＴＡＲＴ２                *
*----------------------------------------------------------*
 AUTHAC-START2-SUB       SECTION.
*
     MOVE    ZERO            TO   AUT-FLG.
     MOVE    SPACE           TO   AUT-REC.
     INITIALIZE                   AUT-REC.
     MOVE    SAV-SOKCD       TO   AUT-F01.
     MOVE    SAV-NKEY-SHIIRE TO   AUT-F02.
     MOVE    SAV-NKEY-SHOCD  TO   AUT-F031.
     MOVE    SAV-NKEY-HINT   TO   AUT-F032.
     MOVE    SAV-NKEY-TANA   TO   AUT-F04.
     START   AUTHACF    KEY  >    AUT-F01
                                  AUT-F02
                                  AUT-F031
                                  AUT-F032
                                  AUT-F04
       INVALID      KEY
             MOVE       1         TO   AUT-FLG
       NOT INVALID  KEY
             MOVE       ZERO      TO   AUT-FLG
     END-START.
 AUTHAC-START2-END.
     EXIT.
*----------------------------------------------------------*
*      2.4.4     自動発注ワークＳＴＡＲＴ３                *
*----------------------------------------------------------*
 AUTHAC-START3-SUB       SECTION.
*
     MOVE    ZERO            TO   AUT-FLG.
     MOVE    SPACE           TO   AUT-REC.
     INITIALIZE                   AUT-REC.
     MOVE    SAV-SOKCD       TO   AUT-F01.
     MOVE    SAV-BKEY-SHIIRE TO   AUT-F02.
     MOVE    SAV-BKEY-SHOCD  TO   AUT-F031.
     MOVE    SAV-BKEY-HINT   TO   AUT-F032.
     MOVE    SAV-BKEY-TANA   TO   AUT-F04.
     START   AUTHACF    KEY  <    AUT-F01
                                  AUT-F02
                                  AUT-F031
                                  AUT-F032
                                  AUT-F04
                        WITH      REVERSED  ORDER
       INVALID      KEY
             MOVE       1         TO   AUT-FLG
       NOT INVALID  KEY
             MOVE       ZERO      TO   AUT-FLG
     END-START.
 AUTHAC-START3-END.
     EXIT.
*----------------------------------------------------------*
*      2.5       確認入力                                  *
*----------------------------------------------------------*
 KAKUNIN-SUB       SECTION.
     PERFORM       MSG-SEC.
     MOVE     PMSG02         TO   PFMSG.
     PERFORM       DSP-WRITE-SUB.
     MOVE    "KAKU"          TO    DSP-GROUP.
     PERFORM       DSP-READ-SUB.
* アテンション判定
     EVALUATE  DSP-FUNC
         WHEN   "F004"
                        MOVE    "1"    TO   MAIN-FLG
                        PERFORM   HEADDEL-SUB
                        PERFORM   BODYDEL-SUB
         WHEN   "F005"
                        MOVE     "END" TO   END-FLG
         WHEN   "F006"
                        MOVE     "2"   TO   MAIN-FLG
         WHEN   "F011"
                        PERFORM   AUTHAC-UPD-SUB
                        PERFORM   BEFORE-PAGE-SUB
         WHEN   "F012"
                        PERFORM   AUTHAC-UPD-SUB
                        PERFORM   NEXT-PAGE-SUB
         WHEN   "E000"
                        PERFORM   KAKUCHK-SUB
         WHEN    OTHER
                        MOVE      01   TO   ERR-MSG-CD
     END-EVALUATE.
 KAKUNIN-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1     確認の入力チェック                        *
*----------------------------------------------------------*
 KAKUCHK-SUB             SECTION.
     MOVE       "M"     TO   EDIT-OPTION  OF  KAKU.
     MOVE       SPACE   TO   EDIT-CURSOR  OF  KAKU.
     MOVE       ZERO    TO   ERR-MSG-CD.
*
     PERFORM       AUTHAC-UPD-SUB.
     PERFORM       HEADDEL-SUB.
     PERFORM       BODYDEL-SUB.
     MOVE    "1"   TO  MAIN-FLG.
*
 KAKUCHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.2  自動発注ワーク更新                       *
*----------------------------------------------------------*
 AUTHAC-UPD-SUB          SECTION.
*    自動発注ワーク更新
     PERFORM  VARYING   IXA       FROM    1  BY  1
              UNTIL     IXA  >    7
         IF   SHOCD(IXA)     NOT =     SPACE
              PERFORM   AUT-READ-SUB
              IF   INVALID-FLG    =    1
                   CONTINUE
              ELSE
                   IF ( KAKUTE(IXA)    NOT =     AUT-F17 ) OR
                      ( HACSU (IXA)    NOT =     AUT-F10 )
                        MOVE KAKUTE(IXA)    TO   AUT-F17
                        MOVE HACSU (IXA)    TO   AUT-F10
                        MOVE TANTO          TO   AUT-F14
                        REWRITE   AUT-REC
                   END-IF
              END-IF
         END-IF
     END-PERFORM.
 AUTHAC-UPD-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF      HJYOKEN   HMEIMS
              ZSHIMS    AUTHACF   ZSOKMS.
 END-END.
     EXIT.
************************************************************
*      9.0        共通処理                                 *
************************************************************
*==========================================================*
*      9.1       エラー メッセージセット                   *
*==========================================================*
 MSG-SEC                SECTION.
     IF  ERR-MSG-CD     =    ZERO
         MOVE      SPACE     TO   ERRMSG
     ELSE
         MOVE      ERR-MSG(ERR-MSG-CD)      TO   ERRMSG
         MOVE      ZERO      TO   ERR-MSG-CD
     END-IF.
 MSG-END.
     EXIT.
*==========================================================*
*      9.2       項目制御部初期化                          *
*==========================================================*
*----------------------------------------------------------*
*      9.2.1     項目制御部初期化（ヘッド部）              *
*----------------------------------------------------------*
 EDIT-SET-HEAD          SECTION.
     MOVE   "M"     TO   EDIT-OPTION  OF  SOKCD.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SOKCD.
     MOVE   "M"     TO   EDIT-OPTION  OF  TANTO.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  TANTO.
     MOVE   "M"     TO   EDIT-OPTION  OF  HSIIRE.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HSIIRE.
 EDIT-SET-HEAD-END.
     EXIT.
*----------------------------------------------------------*
*      9.2.3     項目制御部初期化（ボディ部）              *
*----------------------------------------------------------*
 EDIT-SET-BODY          SECTION.
     PERFORM  VARYING IXB     FROM  1  BY  1
              UNTIL   IXB     >     7
         PERFORM     EDIT-SET-GYO
     END-PERFORM.
 EDIT-SET-BODY-END.
     EXIT.
*----------------------------------------------------------*
*      9.2.3.1   項目制御部初期化（ボディ部１行）          *
*----------------------------------------------------------*
 EDIT-SET-GYO           SECTION.
*
     MOVE   "M"     TO   EDIT-OPTION  OF  KAKUTE(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  KAKUTE(IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  HACSU (IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  HACSU (IXB).
*
 EDIT-SET-GYO-END.
     EXIT.
*==========================================================*
*      9.3       画面表示処理                              *
*==========================================================*
 DSP-WRITE-SUB          SECTION.
     MOVE    "FHA01401"      TO   DSP-FORMAT.
     MOVE    "SCREEN"        TO   DSP-GROUP.
     MOVE     SPACE          TO   DSP-PROC.
     MOVE     HEN-DATE       TO   SDATE.
     MOVE     HEN-TIME       TO   STIME.
     MOVE     HEN-TOKHAN-AREA     TO   TOKHAN.
     WRITE    FHA01401.
 DSP-WRITE-END.
     EXIT.
*==========================================================*
*      9.4       画面データの入力処理                      *
*==========================================================*
 DSP-READ-SUB           SECTION.
     MOVE  "NE"    TO   DSP-PROC.
     READ   DSPF.
 DSP-READ-END.
     EXIT.
*==========================================================*
*      9.5       ＨＥＡＤ部消去                            *
*==========================================================*
 HEADDEL-SUB            SECTION.
     MOVE   SPACE       TO   HEAD0.
     PERFORM            EDIT-SET-HEAD.
 HEADDEL-END.
     EXIT.
*==========================================================*
*      9.6       ＢＯＤＹ部消去                            *
*==========================================================*
 BODYDEL-SUB            SECTION.
     MOVE  SPACE        TO   MEISAI.
     PERFORM            EDIT-SET-BODY.
 BODYDEL-END.
     EXIT.
*==========================================================*
*      9.7       ファイル処理                              *
*==========================================================*
*----------------------------------------------------------*
*      9.7.1     条件ファイルＲＥＡＤ                      *
*----------------------------------------------------------*
 JYO-READ-SUB           SECTION.
     READ    HJYOKEN
       INVALID      KEY
          MOVE     "1"       TO   INVALID-FLG
          INITIALIZE              JYO-REC
       NOT INVALID  KEY
          MOVE      ZERO     TO   INVALID-FLG
     END-READ.
 JYO-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.2     仕入先名の取得                            *
*----------------------------------------------------------*
 SHI-READ1-SUB          SECTION.
     MOVE    HSIIRE          TO   SHI-F01.
     READ    ZSHIMS
       INVALID      KEY
          MOVE     SPACE     TO   SINAME
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     SHI-F02   TO   SINAME
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 SHI-READ1-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.2     仕入先名の取得                            *
*----------------------------------------------------------*
 SHI-READ-SUB           SECTION.
     MOVE    SHIIRE(IXA)     TO   SHI-F01.
     READ    ZSHIMS
       INVALID      KEY
          MOVE     SPACE     TO   SIRENM(IXA)
          MOVE     "1"       TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     SHI-F02   TO   SIRENM(IXA)
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 SHI-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.3     商品名称マスタの検索                      *
*----------------------------------------------------------*
 MEI-READ-SUB           SECTION.
*
     MOVE     SHOCD(IXA)          TO   MEI-F011.
     MOVE     HIN1 (IXA)          TO   MEI-F0121.
     MOVE     HIN2 (IXA)          TO   MEI-F0122.
     MOVE     HIN3 (IXA)          TO   MEI-F0123.
     READ     HMEIMS
         INVALID KEY
              MOVE     "1"        TO   INVALID-FLG
              MOVE      SPACE     TO   SHONM(IXA)
         NOT INVALID  KEY
              MOVE      ZERO      TO   INVALID-FLG
              MOVE      MEI-F02   TO   SHONM(IXA)
     END-READ.
*
 MEI-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.4     自動発注ワークの検索                      *
*----------------------------------------------------------*
 AUT-READ-SUB           SECTION.
*
     MOVE     SPACE               TO   AUT-REC.
     INITIALIZE                        AUT-REC.
     MOVE     SAV-SOKCD           TO   AUT-F01.
     MOVE     SHIIRE  (IXA)       TO   AUT-F02.
     MOVE     SHOCD   (IXA)       TO   AUT-F031.
     MOVE     HIN1    (IXA)       TO   AUT-F032(1:5).
     MOVE     HIN2    (IXA)       TO   AUT-F032(6:2).
     MOVE     HIN3    (IXA)       TO   AUT-F032(8:1).
     MOVE     SAV-TANA(IXA)       TO   AUT-F04.
     READ     AUTHACF
         INVALID KEY
              MOVE     "1"        TO   INVALID-FLG
         NOT INVALID  KEY
              MOVE      ZERO      TO   INVALID-FLG
     END-READ.
*
 AUT-READ-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
